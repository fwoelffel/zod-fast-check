import fc from "fast-check";
import * as z from "zod";
import {
  INVALID,
  OK,
  type ParseInput,
  type ParseReturnType,
  ZodSchema,
  type ZodTypeAny,
  type ZodTypeDef,
} from "zod";

import {
  ZodFastCheck,
  ZodFastCheckGenerationError,
  ZodFastCheckUnsupportedSchemaError,
} from "../src/zod-fast-check";

describe("Generate arbitraries for Zod schema input types", () => {
  enum Biscuits {
    CustardCream = 1,
    Digestive = 0,
    RichTea = 2,
  }

  enum Cakes {
    CarrotCake = "CARROT_CAKE",
    ChocolateCake = "CHOCOLATE_CAKE",
    VictoriaSponge = "VICTORIA_SPONGE",
  }

  const penguinSymbol = Symbol.for("penguin");

  const schemas = {
    any: z.any(),
    "array branded with symbol": z
      .array(z.number())
      .brand<typeof penguinSymbol>(),
    "array of arrays of booleans": z.array(z.array(z.boolean())),
    "array of numbers": z.array(z.number()),
    "array of string": z.array(z.string()),
    bigint: z.bigint(),
    boolean: z.boolean(),
    "Coerced bigint": z.coerce.bigint(),
    "Coerced boolean": z.coerce.boolean(),
    "Coerced date": z.coerce.date(),
    "Coerced number": z.coerce.number(),
    "Coerced string": z.coerce.string(),
    "const enum": z.nativeEnum({
      Duck: "duck",
      Goose: 3,
      Swan: "swan",
    }),
    cuid: z.string().cuid(),
    cuid2: z.string().cuid2(),
    date: z.date(),
    datetime: z.string().datetime(),
    "datetime with high precision": z.string().datetime({ precision: 6 }),
    "datetime with low precision": z.string().datetime({ precision: 0 }),
    "datetime with offset": z.string().datetime({ offset: true }),
    "deeply nested transformer": z.array(z.boolean().transform(Number)),
    "discriminated union": z.discriminatedUnion("type", [
      z.object({ a: z.string(), type: z.literal("a") }),
      z.object({
        b: z.object({
          x: z.string(),
        }),
        type: z.literal("b"),
      }),
      z.object({
        c: z.number(),
        type: z.literal("c"),
      }),
    ]),
    email: z.string().email(),
    "empty object": z.object({}),
    "empty tuple": z.tuple([]),
    enum: z.enum(["Bear", "Wolf", "Fox"]),
    finite: z.number().finite(),
    "function returning boolean": z.function().returns(z.boolean()),
    int: z.number().int(),
    "literal boolean": z.literal(false),
    "literal number": z.literal(123.5),
    "literal string": z.literal("hello"),
    "literal symbol": z.literal(Symbol("mySymbol")),
    "map with object keys": z.map(
      z.object({ id: z.number() }),
      z.array(z.boolean()),
    ),
    "map with string keys": z.map(z.string(), z.number()),
    "multiple multiple of": z.number().multipleOf(3).multipleOf(5),
    "multiple of": z.number().multipleOf(3),
    "multiple of with min and max": z.number().multipleOf(10).min(67).max(99),
    nan: z.nan(),
    "native enum with numeric values": z.nativeEnum(Biscuits),
    "native enum with string values": z.nativeEnum(Cakes),
    negative: z.number().negative(),
    "nested object": z.object({
      child: z.object({
        grandchild1: z.null(),
        grandchild2: z.boolean(),
      }),
    }),
    "nested tuple": z.tuple([z.string(), z.tuple([z.number()])]),
    "nonempty array": z.array(z.number()).nonempty(),
    "nonempty set": z.set(z.number()).nonempty(),
    "nonempty tuple": z.tuple([z.string(), z.boolean(), z.date()]),
    nonnegative: z.number().nonnegative(),
    nonpositive: z.number().nonpositive(),
    null: z.null(),
    "nullable object": z.nullable(z.object({ age: z.number() })),
    "nullable string": z.nullable(z.string()),

    number: z.number(),
    "number to string transformer": z.number().transform(String),
    "number with custom refinement": z.number().refine((x) => x % 3 === 0),
    "number with float max and min": z.number().min(0.5).max(1.5),
    "number with maximum": z.number().max(500),
    // Schemas which rely on refinements
    "number with minimum": z.number().min(500),
    "object branded with number": z.object({ a: z.boolean() }).brand<123>(),
    "optional boolean": z.optional(z.boolean()),
    "optional number": z.optional(z.number()),
    positive: z.number().positive(),
    promise: z.promise(z.string()),
    "record of numbers": z.record(z.number()),
    "record of objects": z.record(z.object({ name: z.string() })),

    "record of strings": z.record(z.string()),
    "record of strings with min-length keys": z.record(
      z.string().min(1),
      z.string(),
    ),
    "record of strings with min-length values": z.record(z.string().min(1)),
    regex: z.string().regex(/\s/),
    set: z.set(z.number()),
    "set with max": z.set(z.number()).max(3),
    "set with min": z.set(z.number()).min(2),
    "simple object": z.object({
      aBoolean: z.boolean(),
      aString: z.string(),
    }),
    string: z.string(),
    "string branded with string": z.string().brand<"timezone">(),
    "string to number pipeline": z
      .string()
      .transform((s) => s.length)
      .pipe(z.number().min(5)),
    "string with catch": z.string().catch("fallback"),
    "string with fixed length": z.string().length(256),
    "string with maximum length": z.string().max(24),
    "string with minimum length": z.string().min(24),
    "string with prefix": z.string().startsWith("prefix"),
    "string with suffix": z.string().endsWith("suffix"),
    symbol: z.symbol(),
    undefined: z.undefined(),
    union: z.union([z.boolean(), z.string()]),
    unknown: z.unknown(),
    url: z.string().url(),
    uuid: z.string().uuid(),
    void: z.void(),
    "with default": z.number().default(0),
  };

  for (const [name, schema] of Object.entries(schemas)) {
    test(name, () => {
      const arbitrary = ZodFastCheck().inputOf(schema);
      return fc.assert(
        fc.asyncProperty(arbitrary, async (value) => {
          await schema.parse(value);
        }),
      );
    });
  }
});

describe("Generate arbitraries for Zod schema output types", () => {
  test("number to string transformer", () => {
    const targetSchema = z.string().refine((s) => !Number.isNaN(+s));
    const schema = z.number().transform(String);

    const arbitrary = ZodFastCheck().outputOf(schema);

    fc.assert(
      fc.property(arbitrary, (value) => {
        targetSchema.parse(value);
      }),
    );
  });

  test("deeply nested transformer", () => {
    const targetSchema = z.array(z.number());
    const schema = z.array(z.boolean().transform(Number));

    const arbitrary = ZodFastCheck().outputOf(schema);

    fc.assert(
      fc.property(arbitrary, (value) => {
        targetSchema.parse(value);
      }),
    );
  });

  test("transformer within a transformer", () => {
    // This schema accepts an array of booleans and converts them
    // to strings with exclamation marks then concatenates them.
    const targetSchema = z.string().regex(/(true!|false!)*/);
    const schema = z
      .array(z.boolean().transform((bool) => `${bool}!`))
      .transform((array) => array.join(""));

    const arbitrary = ZodFastCheck().outputOf(schema);

    fc.assert(
      fc.asyncProperty(arbitrary, async (value) => {
        await targetSchema.parse(value);
      }),
    );
  });

  test("doubling transformer", () => {
    // Above this, doubling the number makes it too big to represent,
    // so it gets rounded to infinity.
    const MAX = 1e307;
    const MIN = -MAX;

    const targetSchema = z
      .number()
      .int()
      .refine((x) => x % 2 === 0);
    const schema = z
      .number()
      .int()
      .refine((x) => x < MAX && x > MIN)
      .transform((x) => x * 2);

    const arbitrary = ZodFastCheck().outputOf(schema);

    fc.assert(
      fc.asyncProperty(arbitrary, async (value) => {
        await targetSchema.parse(value);
      }),
    );
  });

  test("schema with default value", () => {
    // Unlike the input arbitrary, the output arbitrary should never
    // produce "undefined" for a schema with a default.
    const targetSchema = z.string();
    const schema = z.string().default("hello");

    const arbitrary = ZodFastCheck().outputOf(schema);

    fc.assert(
      fc.property(arbitrary, (value) => {
        targetSchema.parse(value);
      }),
    );
  });

  test("string with catch", () => {
    const targetSchema = z.string();
    const schema = z.string().catch("fallback");

    const arbitrary = ZodFastCheck().outputOf(schema);

    fc.assert(
      fc.property(arbitrary, (value) => {
        targetSchema.parse(value);
      }),
    );
  });

  test("trimmed string", () => {
    const schema = z.string().trim();

    const arbitrary = ZodFastCheck().outputOf(schema);

    fc.assert(
      fc.property(arbitrary, (value) => {
        expect(value).not.toMatch(/(^\s)|(\s$)/);
      }),
    );
  });

  test("a branded type schema uses an arbitrary for the underlying schema", () => {
    const schema = z.string().brand<"brand">();
    type BrandedString = z.output<typeof schema>;

    const arbitrary = ZodFastCheck().outputOf(schema);

    fc.assert(
      fc.property(arbitrary, (value: BrandedString) => {
        expect(typeof value).toBe("string");
      }),
    );
  });

  test("string to number pipeline", () => {
    const targetSchema = z.number().min(5).int();
    const schema = z
      .string()
      .transform((s) => s.length)
      .pipe(z.number().min(5));

    const arbitrary = ZodFastCheck().outputOf(schema);

    fc.assert(
      fc.property(arbitrary, (value) => {
        targetSchema.parse(value);
      }),
    );
  });
});

describe("Override the arbitrary for a particular schema type", () => {
  const UUID = z.string().uuid();

  test("using custom UUID arbitrary", () => {
    const arbitrary = ZodFastCheck().override(UUID, fc.uuid()).inputOf(UUID);

    fc.assert(
      fc.property(arbitrary, (value) => {
        UUID.parse(value);
      }),
    );
  });

  test("using custom UUID arbitrary in nested schema", () => {
    const schema = z.object({ ids: z.array(UUID) });

    const arbitrary = ZodFastCheck().override(UUID, fc.uuid()).inputOf(schema);

    fc.assert(
      fc.property(arbitrary, (value) => {
        schema.parse(value);
      }),
    );
  });

  const IntAsString = z.number().int().transform(String);

  test("using custom integer arbitrary for IntAsString input", () => {
    const arbitrary = ZodFastCheck()
      .override(IntAsString, fc.integer())
      .inputOf(IntAsString);

    fc.assert(
      fc.property(arbitrary, (value) => {
        z.number().int().parse(value);
      }),
    );
  });

  test("using custom integer arbitrary for IntAsString output", () => {
    const arbitrary = ZodFastCheck()
      .override(IntAsString, fc.integer())
      .outputOf(IntAsString);

    fc.assert(
      fc.property(arbitrary, (value) => {
        expect(typeof value).toBe("string");
        expect(Number(value) === Number.parseInt(value, 10)).toBe(true);
      }),
    );
  });

  test("using a function to lazily define an override", () => {
    const NumericString = z.string().regex(/^\d+$/);

    const zfc = ZodFastCheck().override(NumericString, (zfc) =>
      zfc.inputOf(z.number().int().nonnegative()).map(String),
    );

    const arbitrary = zfc.outputOf(NumericString);

    fc.assert(
      fc.property(arbitrary, (value) => {
        expect(value).toMatch(/^\d+$/);
      }),
    );
  });
});

describe("Throwing an error if it is not able to generate a value", () => {
  test("generating input values for an impossible refinement", () => {
    const arbitrary = ZodFastCheck().inputOf(z.string().refine(() => false));

    expect(() => {
      fc.assert(
        fc.property(arbitrary, (value) => {
          return true;
        }),
      );
    }).toThrow(
      new ZodFastCheckGenerationError(
        "Unable to generate valid values for Zod schema. " +
          "An override is must be provided for the schema at path '.'.",
      ),
    );
  });

  test("generating output values for an impossible refinement", () => {
    const arbitrary = ZodFastCheck().outputOf(z.string().refine(() => false));

    expect(() => {
      fc.assert(
        fc.property(arbitrary, (value) => {
          return true;
        }),
      );
    }).toThrow(ZodFastCheckGenerationError);
  });

  // Tests for the "paths" given in error messages to locate the problematic
  // sub-schema within a nested schema.

  const impossible = z.string().refine(() => false);

  const cases: {
    description: string;
    expectedErrorPath: string;
    schema: ZodTypeAny;
  }[] = [
    {
      description: "nested objects",
      expectedErrorPath: ".foo.bar",
      schema: z.object({ foo: z.object({ bar: impossible }) }),
    },
    {
      description: "arrays",
      expectedErrorPath: ".items[*]",
      schema: z.object({ items: z.array(impossible) }),
    },
    {
      description: "unions",
      expectedErrorPath: ".status",
      schema: z.object({ status: z.union([z.number(), impossible]) }),
    },
    {
      description: "discriminated unions",
      expectedErrorPath: ".a",
      schema: z.discriminatedUnion("type", [
        z.object({ a: impossible, type: z.literal("a") }),
        z.object({ b: z.string(), type: z.literal("b") }),
      ]),
    },
    {
      description: "tuples",
      expectedErrorPath: ".scores[*]",
      schema: z.object({
        scores: z.record(impossible),
      }),
    },
    {
      description: "map keys",
      expectedErrorPath: ".scores.(key)",
      schema: z.object({
        scores: z.map(impossible, z.number()),
      }),
    },
    {
      description: "map values",
      expectedErrorPath: ".scores.(value)",
      schema: z.object({
        scores: z.map(z.string(), impossible),
      }),
    },
    {
      description: "function return types",
      expectedErrorPath: ".myFunction.(return type)",
      schema: z.object({
        myFunction: z.function(z.tuple([]), impossible),
      }),
    },
    {
      description: "promise resolved types",
      expectedErrorPath: ".myPromise.(resolved type)",
      schema: z.object({
        myPromise: z.promise(impossible),
      }),
    },
    {
      description: "optional types",
      expectedErrorPath: ".myOptional",
      schema: z.object({
        myOptional: z.optional(impossible),
      }),
    },
    {
      description: "nullable types",
      expectedErrorPath: ".myNullable",
      schema: z.object({
        myNullable: z.nullable(impossible),
      }),
    },
    {
      description: "types with defaults",
      expectedErrorPath: ".withDefault",
      schema: z.object({
        withDefault: impossible.default(""),
      }),
    },
    {
      description: "types with transforms",
      expectedErrorPath: ".withTransform",
      schema: z.object({
        withTransform: impossible.transform((s) => !!s),
      }),
    },
  ];

  for (const { description, expectedErrorPath, schema } of cases) {
    test(`correct error path is shown for ${description}`, () => {
      const arbitrary = ZodFastCheck().inputOf(schema);

      expect(() => {
        fc.assert(fc.property(arbitrary, () => true));
      }).toThrow(
        new ZodFastCheckGenerationError(
          `Unable to generate valid values for Zod schema. An override is must be provided for the schema at path '${expectedErrorPath}'.`,
        ),
      );
    });
  }

  test("generating input values for an impossible pipeline", () => {
    const arbitrary = ZodFastCheck().inputOf(z.string().pipe(z.boolean()));

    expect(() => {
      fc.assert(
        fc.property(arbitrary, (value) => {
          return true;
        }),
      );
    }).toThrow(
      new ZodFastCheckGenerationError(
        "Unable to generate valid values for Zod schema. " +
          "An override is must be provided for the schema at path '.'.",
      ),
    );
  });
});

describe("Generate arbitraries for lazy schemas", () => {
  enum Biscuits {
    CustardCream = 1,
    Digestive = 0,
    RichTea = 2,
  }

  enum Cakes {
    CarrotCake = "CARROT_CAKE",
    ChocolateCake = "CHOCOLATE_CAKE",
    VictoriaSponge = "VICTORIA_SPONGE",
  }

  const penguinSymbol = Symbol.for("penguin");

  const schemas = {
    any: z.any(),
    "array branded with symbol": z
      .array(z.number())
      .brand<typeof penguinSymbol>(),
    "array of arrays of booleans": z.array(z.array(z.boolean())),
    "array of numbers": z.array(z.number()),
    "array of string": z.array(z.string()),
    bigint: z.bigint(),
    boolean: z.boolean(),
    "Coerced bigint": z.coerce.bigint(),
    "Coerced boolean": z.coerce.boolean(),
    "Coerced date": z.coerce.date(),
    "Coerced number": z.coerce.number(),
    "Coerced string": z.coerce.string(),
    "const enum": z.nativeEnum({
      Duck: "duck",
      Goose: 3,
      Swan: "swan",
    }),
    cuid: z.string().cuid(),
    cuid2: z.string().cuid2(),
    date: z.date(),
    datetime: z.string().datetime(),
    "datetime with high precision": z.string().datetime({ precision: 6 }),
    "datetime with low precision": z.string().datetime({ precision: 0 }),
    "datetime with offset": z.string().datetime({ offset: true }),
    "deeply nested transformer": z.array(z.boolean().transform(Number)),
    "discriminated union": z.discriminatedUnion("type", [
      z.object({ a: z.string(), type: z.literal("a") }),
      z.object({
        b: z.object({
          x: z.string(),
        }),
        type: z.literal("b"),
      }),
      z.object({
        c: z.number(),
        type: z.literal("c"),
      }),
    ]),
    email: z.string().email(),
    "empty object": z.object({}),
    "empty tuple": z.tuple([]),
    enum: z.enum(["Bear", "Wolf", "Fox"]),
    finite: z.number().finite(),
    "function returning boolean": z.function().returns(z.boolean()),
    int: z.number().int(),
    "literal boolean": z.literal(false),
    "literal number": z.literal(123.5),
    "literal string": z.literal("hello"),
    "literal symbol": z.literal(Symbol("mySymbol")),
    "map with object keys": z.map(
      z.object({ id: z.number() }),
      z.array(z.boolean()),
    ),
    "map with string keys": z.map(z.string(), z.number()),
    "multiple multiple of": z.number().multipleOf(3).multipleOf(5),
    "multiple of": z.number().multipleOf(3),
    "multiple of with min and max": z.number().multipleOf(10).min(67).max(99),
    nan: z.nan(),
    "native enum with numeric values": z.nativeEnum(Biscuits),
    "native enum with string values": z.nativeEnum(Cakes),
    negative: z.number().negative(),
    "nested object": z.object({
      child: z.object({
        grandchild1: z.null(),
        grandchild2: z.boolean(),
      }),
    }),
    "nested tuple": z.tuple([z.string(), z.tuple([z.number()])]),
    "nonempty array": z.array(z.number()).nonempty(),
    "nonempty set": z.set(z.number()).nonempty(),
    "nonempty tuple": z.tuple([z.string(), z.boolean(), z.date()]),
    nonnegative: z.number().nonnegative(),
    nonpositive: z.number().nonpositive(),
    null: z.null(),
    "nullable object": z.nullable(z.object({ age: z.number() })),
    "nullable string": z.nullable(z.string()),

    number: z.number(),
    "number to string transformer": z.number().transform(String),
    "number with custom refinement": z.number().refine((x) => x % 3 === 0),
    "number with float max and min": z.number().min(0.5).max(1.5),
    "number with maximum": z.number().max(500),
    // Schemas which rely on refinements
    "number with minimum": z.number().min(500),
    "object branded with number": z.object({ a: z.boolean() }).brand<123>(),
    "optional boolean": z.optional(z.boolean()),
    "optional number": z.optional(z.number()),
    positive: z.number().positive(),
    promise: z.promise(z.string()),
    "record of numbers": z.record(z.number()),
    "record of objects": z.record(z.object({ name: z.string() })),

    "record of strings": z.record(z.string()),
    "record of strings with min-length keys": z.record(
      z.string().min(1),
      z.string(),
    ),
    "record of strings with min-length values": z.record(z.string().min(1)),
    regex: z.string().regex(/\s/),
    set: z.set(z.number()),
    "set with max": z.set(z.number()).max(3),
    "set with min": z.set(z.number()).min(2),
    "simple object": z.object({
      aBoolean: z.boolean(),
      aString: z.string(),
    }),
    string: z.string(),
    "string branded with string": z.string().brand<"timezone">(),
    "string to number pipeline": z
      .string()
      .transform((s) => s.length)
      .pipe(z.number().min(5)),
    "string with catch": z.string().catch("fallback"),
    "string with fixed length": z.string().length(256),
    "string with maximum length": z.string().max(24),
    "string with minimum length": z.string().min(24),
    "string with prefix": z.string().startsWith("prefix"),
    "string with suffix": z.string().endsWith("suffix"),
    symbol: z.symbol(),
    undefined: z.undefined(),
    union: z.union([z.boolean(), z.string()]),
    unknown: z.unknown(),
    url: z.string().url(),
    uuid: z.string().uuid(),
    void: z.void(),
    "with default": z.number().default(0),
  };

  for (const [name, schema] of Object.entries(schemas)) {
    test(name, () => {
      const lazySchema = z.lazy(() => schema);
      const arbitrary = ZodFastCheck().inputOf(lazySchema);
      return fc.assert(
        fc.asyncProperty(arbitrary, async (value) => {
          await schema.parse(value);
        }),
      );
    });
  }
});

describe("Throwing an error if the schema type is not supported", () => {
  test("never schemas", () => {
    expect(() => ZodFastCheck().inputOf(z.never())).toThrow(
      new ZodFastCheckUnsupportedSchemaError(
        "Unable to generate valid values for Zod schema. " +
          "Never schemas are not supported (at path '.').",
      ),
    );
  });

  test("intersection schemas", () => {
    expect(() =>
      ZodFastCheck().inputOf(
        z.intersection(
          z.object({ foo: z.string() }),
          z.object({ bar: z.number() }),
        ),
      ),
    ).toThrow(
      new ZodFastCheckUnsupportedSchemaError(
        "Unable to generate valid values for Zod schema. " +
          "Intersection schemas are not supported (at path '.').",
      ),
    );
  });

  test("third-party schemas", () => {
    interface ZodSymbolDef extends ZodTypeDef {
      symbol: symbol;
    }

    class SymbolSchema extends ZodSchema<symbol, ZodSymbolDef, symbol> {
      _parse({ data }: ParseInput): ParseReturnType<symbol> {
        if (data === this._def.symbol) {
          return OK(data);
        }
        return INVALID;
      }
    }

    expect(() =>
      ZodFastCheck().inputOf(new SymbolSchema({ symbol: Symbol.iterator })),
    ).toThrow(
      new ZodFastCheckUnsupportedSchemaError(
        "Unable to generate valid values for Zod schema. " +
          "'SymbolSchema' schemas are not supported (at path '.').",
      ),
    );
  });
});
