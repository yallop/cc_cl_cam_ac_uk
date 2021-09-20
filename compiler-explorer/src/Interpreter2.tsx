import { useEffect, useState } from "react";

import Editor from "./Editor";
import Progress, { keyHandler } from "./Progress";
import { i2Stream, Stream, Error } from "./slangWrapper";
import { SubViewProps } from "./App";

import "./Stacks.css";

type CodeStack = [number, string][];
type EnvStack = string[];
type Memory = string[];
export type Steps = [CodeStack, EnvStack, Memory][];

const trimClosures = (ss: string[]) => {
  return ss.map((s, _) =>
    s.length > 40 ? s.split("\n").join(" ").slice(0, 40) + "..." : s
  );
};

const ErrorWrapper = (props: SubViewProps) => {
  const { source } = props;
  const [stream, setStream] = useState<Stream<Steps> | Error>(i2Stream(source));

  useEffect(() => setStream(i2Stream(source)), [source]);

  if ("error" in stream) {
    return <div className="error">{stream.error}</div>;
  }

  return (
    <Interpreter2
      {...props}
      stream={stream}
      setStream={setStream}
    ></Interpreter2>
  );
};

interface I2Props extends SubViewProps {
  stream: Stream<Steps>;
  setStream: (s: Stream<Steps>) => void;
}

const Interpreter2 = ({
  source,
  onMouseMove,
  onMouseLeave,
  decorations,
  setStream,
  stream,
}: I2Props) => {
  const [step, setStep] = useState(0);

  const [showNestedInstructions, setShowNestedInstructions] = useState(true);

  useEffect(() => {
    setStep(0);
  }, [source]);

  const { steps, next } = stream;
  const [codeStack, envStack, memory] = steps[step];

  const filteredCodeStack = showNestedInstructions
    ? codeStack
    : codeStack.filter(([i, s]) => !s.startsWith("\t"));

  const codeStackS = filteredCodeStack
    .map(([_, s]) => s)
    .join("\n")
    .slice(1, -1);

  const envStackS = (
    showNestedInstructions ? envStack : trimClosures(envStack)
  ).join("\n");

  const memoryS = memory.join("\n");

  const showMem = steps.some(([_, __, s]) => s.length > 0);

  const handler = keyHandler(step, setStep, steps.length);

  useEffect(() => {
    if (step >= steps.length - 3 && steps[steps.length - 1][0].length > 0) {
      setStream(next());
    }
  }, [step, steps, next, setStream]);

  return (
    <div className="interpreter">
      <div className="interpreterTitle">
        <h3>
          Step {step} - {step > 0 ? steps[step - 1][0][0] : "Start"}
        </h3>
        <button
          onClick={() => setShowNestedInstructions(!showNestedInstructions)}
        >
          {showNestedInstructions ? "Hide" : "Show"} nested instructions
        </button>
      </div>
      <div className="interpreterEditors">
        <Editor
          value={codeStackS}
          decorations={decorations(filteredCodeStack)}
          onMouseMove={onMouseMove(filteredCodeStack)}
          onMouseLeave={onMouseLeave}
          language="javascript"
          onKeyDown={(e) => handler(e.key)}
          options={{
            readOnly: true,
            theme: "vs-dark",
            lineNumbers: (lineNumber: number) =>
              codeStackS.split("\n").length - lineNumber + 1,
          }}
        />
        <Editor
          value={envStackS}
          className="i2StackEditor"
          language="javascript"
          onKeyDown={(e) => handler(e.key)}
          options={{
            readOnly: true,
            theme: "vs-dark",
            lineNumbers: (lineNumber: number) =>
              envStackS.split("\n").length - lineNumber + 1,
          }}
        />
        {showMem ? (
          <Editor
            value={memoryS}
            language="javascript"
            onKeyDown={(e) => handler(e.key)}
            options={{
              readOnly: true,
              theme: "vs-dark",
              lineNumbers: (lineNumber: number) => (lineNumber - 1).toString(),
            }}
          />
        ) : null}
      </div>
      <Progress values={steps} index={step} setIndex={setStep} />
    </div>
  );
};

export default ErrorWrapper;
