import { jargonStepsT } from "./InterpreterJargon";

export type Code = [number, string][];
export type CompilerResult =
  | { code: null; error: string }
  | { code: Code; error: null };

//@ts-ignore
export const interp = (s: string) => slang.interp0(s);

const parseResult = (result: string): CompilerResult => {
  if (result.trim().startsWith("ERROR")) {
    return { code: null, error: result };
  }

  try {
    //@ts-ignore
    return { code: JSON.parse(result), error: null };
  } catch (e) {
    return { code: null, error: (e as Error).message };
  }
};

export const i2compile = (s: string): CompilerResult => {
  return parseResult(slang.interp2Code(s));
};

export const i3compile = (s: string): CompilerResult => {
  return parseResult(slang.interp3Code(s));
};

export const jargonCompile = (s: string): CompilerResult => {
  return parseResult(slang.jargonCode(s));
};

export const computeI2steps = (s: string): [string[], string[], string[]][] => {
  //@ts-ignore
  return JSON.parse(slang.interp2(s));
};

export const computeI3steps = (
  s: string
): [string, [number, string[], string[]][]] => {
  //@ts-ignore
  return JSON.parse(slang.interp3(s));
};

export const computeJargonSteps = (s: string): jargonStepsT => {
  //@ts-ignore
  return JSON.parse(slang.jargon(s));
};

export function stringOfCode(c: Code): string {
  return c.map(([_, s]) => s).join("\n");
}

export function highlightRowsForLocation(c: Code, l: number): number[] {
  return c.reduce(
    (linesToHighlight, codeLine, i) =>
      codeLine[0] === l ? [i, ...linesToHighlight] : linesToHighlight,
    [] as number[]
  );
}
