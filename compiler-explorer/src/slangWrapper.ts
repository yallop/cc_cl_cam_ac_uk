import { Steps as i2Steps } from "./Interpreter2";
import { StreamWrapper as I3StreamWrapper } from "./Interpreter3";
import { StreamWrapper as JargonStreamWrapper } from "./InterpreterJargon";

export type code = [number, string][];

export type Error = {
  error: string;
};

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

export const i2Stream = (s: string): Stream<i2Steps> | Error => {
  //@ts-ignore
  const stream = slang.i2Stream(s);
  if (stream.steps.startsWith("Error")) return { error: stream.steps };
  return jsonParsedStream(stream);
};

export const i3Stream = (s: string): I3StreamWrapper | Error => {
  //@ts-ignore
  const streamWrapper = slang.i3Stream(s);
  if (streamWrapper.code.startsWith("Error"))
    return { error: streamWrapper.code };
  const { code, stepStream } = streamWrapper;
  return { code: JSON.parse(code), stepStream: jsonParsedStream(stepStream) };
};

export const jargonStream = (s: string): JargonStreamWrapper | Error => {
  //@ts-ignore
  const streamWrapper = slang.jargonStream(s);
  if (streamWrapper.code.startsWith("Error"))
    return { error: streamWrapper.code };
  const { code, stepStream } = streamWrapper;
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
