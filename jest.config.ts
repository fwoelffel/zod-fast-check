import { createDefaultEsmPreset, type JestConfigWithTsJest } from "ts-jest";

const presetConfig = createDefaultEsmPreset({
  tsconfig: "./tests/tsconfig.json",
});

const jestConfig: JestConfigWithTsJest = {
  ...presetConfig,
};

export default jestConfig;
