import type {
  input,
  output,
  ZodArray,
  ZodBranded,
  ZodCatch,
  ZodDefault,
  ZodDiscriminatedUnion,
  ZodDiscriminatedUnionOption,
  ZodEffects,
  ZodEnum,
  ZodFirstPartySchemaTypes,
  ZodFirstPartyTypeKind,
  ZodFunction,
  ZodLazy,
  ZodLiteral,
  ZodMap,
  ZodNativeEnum,
  ZodNullable,
  ZodNumber,
  ZodObject,
  ZodOptional,
  ZodPipeline,
  ZodPromise,
  ZodRawShape,
  ZodRecord,
  ZodSchema,
  ZodSet,
  ZodString,
  ZodSymbol,
  ZodTuple,
  ZodTypeAny,
  ZodTypeDef,
  ZodUnion,
} from "zod";

import fc, { type Arbitrary } from "fast-check";

const MIN_SUCCESS_RATE = 0.01;

// ZodSymbol is missing from the union for first-party types, so we use this
// type instead which includes it.
type AllFirstPartySchemaTypes = ZodFirstPartySchemaTypes | ZodSymbol;

type ArbitraryBuilder<Schema extends UnknownZodSchema> = (
  schema: Schema,
  path: string,
  recurse: SchemaToArbitrary,
) => Arbitrary<Schema["_input"]>;

type ArbitraryBuilders = {
  [TypeName in ZodFirstPartyTypeKind]: ArbitraryBuilder<
    ExtractFirstPartySchemaType<TypeName>
  >;
};

type ExtractFirstPartySchemaType<TypeName extends ZodFirstPartyTypeKind> =
  Extract<AllFirstPartySchemaTypes, { _def: { typeName: TypeName } }>;

type SchemaToArbitrary = <Schema extends UnknownZodSchema>(
  schema: Schema,
  path: string,
) => Arbitrary<Schema["_input"]>;

type UnknownZodSchema = ZodSchema<unknown, ZodTypeDef, unknown>;

const SCALAR_TYPES = new Set<`${ZodFirstPartyTypeKind}`>([
  "ZodAny",
  "ZodBigInt",
  "ZodBoolean",
  "ZodDate",
  "ZodEnum",
  "ZodLiteral",
  "ZodNativeEnum",
  "ZodNull",
  "ZodNumber",
  "ZodString",
  "ZodUndefined",
  "ZodUnknown",
  "ZodVoid",
]);

export type ZodFastCheck = _ZodFastCheck;

type OverrideArbitrary<Input = unknown> =
  | ((zfc: ZodFastCheck) => Arbitrary<Input>)
  | Arbitrary<Input>;

// eslint-disable-next-line sonarjs/class-name
class _ZodFastCheck {
  private overrides = new Map<
    ZodSchema<unknown, ZodTypeDef, unknown>,
    OverrideArbitrary
  >();

  /**
   * Creates an arbitrary which will generate valid inputs to the schema.
   */
  inputOf<Schema extends UnknownZodSchema>(
    schema: Schema,
  ): Arbitrary<input<Schema>> {
    return this.inputWithPath(schema, "");
  }

  /**
   * Creates an arbitrary which will generate valid parsed outputs of
   * the schema.
   */
  outputOf<Schema extends UnknownZodSchema>(
    schema: Schema,
  ): Arbitrary<output<Schema>> {
    const inputArbitrary = this.inputOf(schema);

    // For scalar types, the input is always the same as the output,
    // so we can just use the input arbitrary unchanged.
    if (
      isFirstPartyType(schema) &&
      SCALAR_TYPES.has((schema as AllFirstPartySchemaTypes)._def.typeName)
    ) {
      return inputArbitrary;
    }

    return inputArbitrary
      .map((value) => schema.safeParse(value))
      .filter(
        throwIfSuccessRateBelow(
          MIN_SUCCESS_RATE,
          isUnionMember({ success: true }),
          "",
        ),
      )
      .map((parsed) => parsed.data);
  }

  /**
   * Returns a new `ZodFastCheck` instance which will use the provided
   * arbitrary when generating inputs for the given schema.
   */
  override<Schema extends UnknownZodSchema>(
    schema: Schema,
    arbitrary: OverrideArbitrary<input<Schema>>,
  ): ZodFastCheck {
    const withOverride = this.clone();
    withOverride.overrides.set(schema, arbitrary);
    return withOverride;
  }

  private clone(): ZodFastCheck {
    const cloned = new _ZodFastCheck();
    for (const [schema, arbitrary] of this.overrides.entries()) {
      cloned.overrides.set(schema, arbitrary);
    }
    return cloned;
  }

  private findOverride<Input>(
    schema: ZodSchema<unknown, ZodTypeDef, Input>,
  ): Arbitrary<Input> | null {
    const override = this.overrides.get(schema);

    if (override) {
      return (
        typeof override === "function" ? override(this) : override
      ) as Arbitrary<Input>;
    }

    return null;
  }

  private inputWithPath<Input>(
    schema: ZodSchema<unknown, ZodTypeDef, Input>,
    path: string,
  ): Arbitrary<Input> {
    const override = this.findOverride(schema);

    if (override) {
      return override;
    }

    if (isFirstPartyType(schema)) {
      const builder = arbitraryBuilders[schema._def.typeName];

      return builder(schema, path, this.inputWithPath.bind(this));
    }

    unsupported(`'${schema.constructor.name}'`, path);
  }
}

// Wrapper function to allow instantiation without "new"
export function ZodFastCheck(): ZodFastCheck {
  return new _ZodFastCheck();
}
// eslint-disable-next-line @typescript-eslint/no-namespace
export declare namespace ZodFastCheck {
  export let prototype: _ZodFastCheck;
}

// Reassign the wrapper function's prototype to ensure
// "instanceof" works as expected.
ZodFastCheck.prototype = _ZodFastCheck.prototype;

function isFirstPartyType(
  schema: UnknownZodSchema,
): schema is AllFirstPartySchemaTypes {
  const typeName = (schema._def as { typeName?: string }).typeName;
  return (
    !!typeName &&
    Object.prototype.hasOwnProperty.call(arbitraryBuilders, typeName)
  );
}

const arbitraryBuilders: ArbitraryBuilders = {
  ZodAny() {
    return fc.anything();
  },
  ZodArray(
    schema: ZodArray<UnknownZodSchema>,
    path: string,
    recurse: SchemaToArbitrary,
  ) {
    const minLength = schema._def.minLength?.value ?? 0;
    const maxLength = Math.min(schema._def.maxLength?.value ?? 10, 10);
    return fc.array(recurse(schema._def.type, `${path}[*]`), {
      maxLength,
      minLength,
    });
  },
  ZodBigInt() {
    return fc.bigInt();
  },
  ZodBoolean() {
    return fc.boolean();
  },
  ZodBranded(
    schema: ZodBranded<UnknownZodSchema, number | string | symbol>,
    path: string,
    recurse: SchemaToArbitrary,
  ) {
    return recurse(schema.unwrap(), path);
  },
  ZodCatch(
    schema: ZodCatch<UnknownZodSchema>,
    path: string,
    recurse: SchemaToArbitrary,
  ) {
    return fc.oneof(recurse(schema._def.innerType, path), fc.anything());
  },
  ZodDate() {
    return fc.date({ noInvalidDate: true });
  },
  ZodDefault(
    schema: ZodDefault<UnknownZodSchema>,
    path: string,
    recurse: SchemaToArbitrary,
  ) {
    return fc.oneof(
      fc.constant(undefined),
      recurse(schema._def.innerType, path),
    );
  },
  ZodDiscriminatedUnion(
    schema: ZodDiscriminatedUnion<
      string,
      ZodDiscriminatedUnionOption<string>[]
    >,
    path: string,
    recurse: SchemaToArbitrary,
  ) {
    const optionsMap = schema._def.optionsMap;

    // eslint-disable-next-line sonarjs/no-alphabetical-sort
    const keys = [...optionsMap.keys()].toSorted();

    return fc.oneof(
      ...keys.map((discriminator) => {
        const option = optionsMap.get(discriminator);
        if (option === undefined) {
          throw new Error(
            `${String(
              discriminator,
            )} should correspond to a variant discriminator, but it does not`,
          );
        }
        return recurse(option, path);
      }),
    );
  },
  ZodEffects(
    schema: ZodEffects<UnknownZodSchema>,
    path: string,
    recurse: SchemaToArbitrary,
  ) {
    const preEffectsArbitrary = recurse(schema._def.schema, path);

    return filterArbitraryBySchema(preEffectsArbitrary, schema, path);
  },
  ZodEnum(schema: ZodEnum<[string, ...string[]]>) {
    return fc.oneof(...schema._def.values.map((value) => fc.constant(value)));
  },
  ZodFunction(
    schema: ZodFunction<ZodTuple, UnknownZodSchema>,
    path: string,
    recurse: SchemaToArbitrary,
  ) {
    return recurse(schema._def.returns, `${path}.(return type)`).map(
      (returnValue) => () => returnValue,
    );
  },
  ZodIntersection(_, path: string) {
    unsupported("Intersection", path);
  },
  ZodLazy(lazy: ZodLazy<ZodTypeAny>, path: string, recurse: SchemaToArbitrary) {
    return recurse(lazy.schema, `${path}.(schema)`);
  },
  ZodLiteral(schema: ZodLiteral<unknown>) {
    return fc.constant(schema._def.value);
  },
  ZodMap(schema: ZodMap, path: string, recurse: SchemaToArbitrary) {
    const key = recurse(schema._def.keyType, `${path}.(key)`);
    const value = recurse(schema._def.valueType, `${path}.(value)`);
    return fc.array(fc.tuple(key, value)).map((entries) => new Map(entries));
  },
  ZodNaN() {
    // This should really be doing some thing like
    // Arbitrary IEEE754 NaN -> DataView -> Number (NaN)
    return fc.constant(Number.NaN);
  },
  ZodNativeEnum(schema: ZodNativeEnum<any>) {
    // eslint-disable-next-line @typescript-eslint/no-unsafe-argument
    const enumValues = getValidEnumValues(schema._def.values);
    return fc.oneof(...enumValues.map((value) => fc.constant(value)));
  },
  ZodNever(_: unknown, path: string) {
    unsupported("Never", path);
  },
  ZodNull() {
    return fc.constant(null);
  },
  ZodNullable(
    schema: ZodNullable<UnknownZodSchema>,
    path: string,
    recurse: SchemaToArbitrary,
  ) {
    const nil = null;
    return fc.option(recurse(schema._def.innerType, path), {
      freq: 2,
      nil,
    });
  },
  ZodNumber(schema: ZodNumber) {
    let min = Number.MIN_SAFE_INTEGER;
    let max = Number.MAX_SAFE_INTEGER;
    let isNumberFinite = false;
    let multipleOf: null | number = null;

    for (const check of schema._def.checks) {
      switch (check.kind) {
        case "finite": {
          isNumberFinite = true;
          break;
        }
        case "int": {
          multipleOf ??= 1;
          break;
        }
        case "max": {
          isNumberFinite = true;
          max = Math.min(
            max,
            check.inclusive ? check.value : check.value - 0.001,
          );
          break;
        }
        case "min": {
          min = Math.max(
            min,
            check.inclusive ? check.value : check.value + 0.001,
          );
          break;
        }
        case "multipleOf": {
          multipleOf = (multipleOf ?? 1) * check.value;
          break;
        }
      }
    }

    if (multipleOf === null) {
      const finiteArb = fc.double({
        max,
        min,
        // fast-check 3 considers NaN to be a Number by default,
        // but Zod does not consider NaN to be a Number
        // see https://github.com/dubzzz/fast-check/blob/main/packages/fast-check/MIGRATION_2.X_TO_3.X.md#new-floating-point-arbitraries-
        noNaN: true,
      });

      return isNumberFinite
        ? finiteArb
        : fc.oneof(finiteArb, fc.constant(Number.POSITIVE_INFINITY));
    }

    const factor = multipleOf;
    return fc
      .integer({
        max: Math.floor(max / factor),
        min: Math.ceil(min / factor),
      })
      .map((x) => x * factor);
  },
  ZodObject(
    schema: ZodObject<ZodRawShape>,
    path: string,
    recurse: SchemaToArbitrary,
  ) {
    const propertyArbitraries = objectFromEntries(
      Object.entries(schema._def.shape()).map(([property, propSchema]) => [
        property,
        recurse(propSchema, `${path}.${property}`),
      ]),
    );
    return fc.record(propertyArbitraries);
  },
  ZodOptional(
    schema: ZodOptional<UnknownZodSchema>,
    path: string,
    recurse: SchemaToArbitrary,
  ) {
    const nil = undefined;
    return fc.option(recurse(schema._def.innerType, path), {
      freq: 2,
      nil,
    });
  },
  ZodPipeline(
    schema: ZodPipeline<UnknownZodSchema, UnknownZodSchema>,
    path: string,
    recurse: SchemaToArbitrary,
  ) {
    return recurse(schema._def.in, path).filter(
      throwIfSuccessRateBelow(
        MIN_SUCCESS_RATE,
        (value): value is typeof value => schema.safeParse(value).success,
        path,
      ),
    );
  },
  ZodPromise(
    schema: ZodPromise<UnknownZodSchema>,
    path: string,
    recurse: SchemaToArbitrary,
  ) {
    return recurse(schema._def.type, `${path}.(resolved type)`).map((value) =>
      Promise.resolve(value),
    );
  },
  ZodReadonly() {
    throw new Error("Function not implemented.");
  },
  ZodRecord(schema: ZodRecord, path: string, recurse: SchemaToArbitrary) {
    return fc.dictionary(
      recurse(schema._def.keyType, path),
      recurse(schema._def.valueType, `${path}[*]`),
      {
        noNullPrototype: true,
      },
    );
  },
  ZodSet(schema: ZodSet, path: string, recurse: SchemaToArbitrary) {
    const minLength = schema._def.minSize?.value ?? 0;
    const maxLength = Math.min(schema._def.maxSize?.value ?? 10, 10);

    return fc
      .uniqueArray(recurse(schema._def.valueType, `${path}.(value)`), {
        maxLength,
        minLength,
      })
      .map((members) => new Set(members));
  },
  ZodString(schema: ZodString, path: string) {
    let minLength = 0;
    let maxLength: null | number = null;
    let hasUnsupportedCheck = false;
    const mappings: ((s: string) => string)[] = [];

    for (const check of schema._def.checks) {
      switch (check.kind) {
        case "cuid": {
          return createCuidArb();
        }
        case "datetime": {
          return createDatetimeStringArb(schema, check);
        }
        case "email": {
          return fc.emailAddress();
        }
        case "endsWith": {
          mappings.push((s) => s + check.value);
          break;
        }
        case "length": {
          minLength = check.value;
          maxLength = check.value;
          break;
        }
        case "max": {
          maxLength = Math.min(
            maxLength ?? Number.POSITIVE_INFINITY,
            check.value,
          );
          break;
        }
        case "min": {
          minLength = Math.max(minLength, check.value);
          break;
        }
        case "startsWith": {
          mappings.push((s) => check.value + s);
          break;
        }
        case "trim": {
          mappings.push((s) => s.trim());
          break;
        }
        case "url": {
          return fc.webUrl();
        }
        case "uuid": {
          return fc.uuid();
        }
        default: {
          hasUnsupportedCheck = true;
        }
      }
    }

    maxLength ??= 2 * minLength + 10;

    let unfiltered = fc.string({
      maxLength,
      minLength,
    });

    for (const mapping of mappings) {
      unfiltered = unfiltered.map(mapping);
    }

    return hasUnsupportedCheck
      ? filterArbitraryBySchema(unfiltered, schema, path)
      : unfiltered;
  },
  ZodSymbol() {
    return fc.string().map(Symbol);
  },
  ZodTuple(schema: ZodTuple, path: string, recurse: SchemaToArbitrary) {
    return fc.tuple(
      ...schema._def.items.map((item, index) =>
        recurse(item, `${path}[${index}]`),
      ),
    );
  },
  ZodUndefined() {
    return fc.constant(undefined);
  },
  ZodUnion(
    schema: ZodUnion<[UnknownZodSchema, ...UnknownZodSchema[]]>,
    path: string,
    recurse: SchemaToArbitrary,
  ) {
    return fc.oneof(
      ...schema._def.options.map((option) => recurse(option, path)),
    );
  },
  ZodUnknown() {
    return fc.anything();
  },
  ZodVoid() {
    return fc.constant(undefined);
  },
};

export class ZodFastCheckError extends Error {}

export class ZodFastCheckGenerationError extends ZodFastCheckError {}

export class ZodFastCheckUnsupportedSchemaError extends ZodFastCheckError {}

// based on the rough spec provided here: https://github.com/paralleldrive/cuid
function createCuidArb(): Arbitrary<string> {
  return fc
    .tuple(
      fc.string({ maxLength: 8, minLength: 8, unit: hexa() }),
      fc
        .integer({ max: 9999, min: 0 })
        .map((n) => n.toString().padStart(4, "0")),
      fc.string({ maxLength: 4, minLength: 4, unit: hexa() }),
      fc.string({ maxLength: 8, minLength: 8, unit: hexa() }),
    )
    .map(
      ([timestamp, counter, fingerprint, random]) =>
        `c${timestamp}${counter}${fingerprint}${random}`,
    );
}

function createDatetimeStringArb(
  schema: ZodString,
  check: { offset: boolean; precision: null | number },
): Arbitrary<string> {
  let arb = fc
    .date({
      max: new Date("9999-12-31T23:59:59Z"),
      min: new Date("0000-01-01T00:00:00Z"),
      noInvalidDate: true,
    })
    .map((date) => date.toISOString());

  if (check.precision === 0) {
    arb = arb.map((utcIsoDatetime) => utcIsoDatetime.replace(/\.\d+Z$/, "Z"));
  } else if (check.precision !== null) {
    const precision = check.precision;
    arb = arb.chain((utcIsoDatetime) =>
      fc
        .integer({ max: 10 ** precision - 1, min: 0 })
        .map((x) => x.toString().padStart(precision, "0"))
        .map((fractionalDigits) =>
          utcIsoDatetime.replace(/\.\d+Z$/, `.${fractionalDigits}Z`),
        ),
    );
  }

  if (check.offset) {
    // Add an arbitrary timezone offset on, if the schema supports it.
    // UTC−12:00 is the furthest behind UTC, UTC+14:00 is the furthest ahead.
    // This does not generate offsets for half-hour and 15 minute timezones.
    arb = arb.chain((utcIsoDatetime) =>
      fc.integer({ max: +14, min: -12 }).map((offsetHours) => {
        if (offsetHours === 0) {
          return utcIsoDatetime;
        }

        const sign = offsetHours > 0 ? "+" : "-";
        const paddedHours = Math.abs(offsetHours).toString().padStart(2, "0");
        return utcIsoDatetime.replace(/Z$/, `${sign}${paddedHours}:00`);
      }),
    );
  }

  return arb;
}

function hexa(): fc.Arbitrary<string> {
  const items = "0123456789abcdef";
  return fc.integer({ max: 15, min: 0 }).map((n) => items.at(n) ?? "0");
}

function unsupported(schemaTypeName: string, path: string): never {
  throw new ZodFastCheckUnsupportedSchemaError(
    `Unable to generate valid values for Zod schema. ${schemaTypeName} schemas are not supported (at path '${path || "."}').`,
  );
}

/**
 * Returns a type guard which filters one member from a union type.
 */
const isUnionMember =
  <T, Filter extends Partial<T>>(filter: Filter) =>
  (value: T): value is Extract<T, Filter> => {
    return Object.entries(filter).every(
      ([key, expected]) => value[key as keyof T] === expected,
    );
  };

function filterArbitraryBySchema<T>(
  arbitrary: Arbitrary<T>,
  schema: ZodSchema<unknown, ZodTypeDef, T>,
  path: string,
): Arbitrary<T> {
  return arbitrary.filter(
    throwIfSuccessRateBelow(
      MIN_SUCCESS_RATE,
      (value): value is typeof value => schema.safeParse(value).success,
      path,
    ),
  );
}

function objectFromEntries<Value>(
  entries: [string, Value][],
): Record<string, Value> {
  const object: Record<string, Value> = {};
  for (const [key, value] of entries) {
    object[key] = value;
  }
  return object;
}

function throwIfSuccessRateBelow<Value, Refined extends Value>(
  rate: number,
  predicate: (value: Value) => value is Refined,
  path: string,
): (value: Value) => value is Refined {
  const MIN_RUNS = 1000;

  let successful = 0;
  let total = 0;

  return (value: Value): value is Refined => {
    const isSuccess = predicate(value);

    total += 1;
    if (isSuccess) successful += 1;

    if (total > MIN_RUNS && successful / total < rate) {
      throw new ZodFastCheckGenerationError(
        `Unable to generate valid values for Zod schema. An override is must be provided for the schema at path '${
          path || "."
        }'.`,
      );
    }

    return isSuccess;
  };
}

const getValidEnumValues = (
  obj: Record<number | string, number | string>,
): unknown[] => {
  const validKeys = Object.keys(obj).filter(
    // @ts-expect-error no explanation
    (key) => typeof obj[obj[key]] !== "number",
  );
  const filtered: Record<string, number | string> = {};
  for (const key of validKeys) {
    // @ts-expect-error no explanation
    filtered[key] = obj[key];
  }
  return Object.values(filtered);
};
