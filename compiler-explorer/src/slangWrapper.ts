import { jargonStepsT } from "./InterpreterJargon";

export type code = [number, string][];

//@ts-ignore
export const interp = (s: string) => slang.interp0(s);

export const i2compile = (s: string): code =>
  //@ts-ignore
  JSON.parse(slang.interp2Code(s));

export const i3compile = (s: string): code =>
  //@ts-ignore
  JSON.parse(slang.interp3Code(s));

export const jargonCompile = (s: string): code =>
  //@ts-ignore
  JSON.parse(slang.jargonCode(s));

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

export function stringOfCode(c: code) {
  return c.map(([_, s]) => s).join("\n");
}

export function highlightRowsForLocation(c: code, l: number): number[] {
  return c.reduce(
    (linesToHighlight, codeLine, i) =>
      codeLine[0] === l ? [i, ...linesToHighlight] : linesToHighlight,
    [] as number[]
  );
}
