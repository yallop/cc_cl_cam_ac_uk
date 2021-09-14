import { Steps as i2Steps } from "./Interpreter2";
import { StreamWrapper as I3StreamWrapper } from "./Interpreter3";
import { StreamWrapper as JargonStreamWrapper } from "./InterpreterJargon";

export type code = [number, string][];

export type Stream<T> = {
  steps: T;
  next: () => Stream<T>;
};

function jsonParsedStream<T>(s: Stream<string>): Stream<T> {
  return {
    steps: JSON.parse(s.steps).reverse(),
    next: () => jsonParsedStream(s.next()),
  };
}

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

export const i2Stream = (s: string): Stream<i2Steps> => {
  //@ts-ignore
  return jsonParsedStream(slang.i2Stream(s));
};

export const i3Stream = (s: string): I3StreamWrapper => {
  //@ts-ignore
  const { code, stepStream } = slang.i3Stream(s);
  return { code: JSON.parse(code), stepStream: jsonParsedStream(stepStream) };
};

export const jargonStream = (s: string): JargonStreamWrapper => {
  //@ts-ignore
  const { code, stepStream } = slang.jargonStream(s);
  return { code: JSON.parse(code), stepStream: jsonParsedStream(stepStream) };
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
