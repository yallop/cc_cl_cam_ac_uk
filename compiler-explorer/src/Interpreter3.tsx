import { useEffect, useState } from "react";

import { Monaco } from "@monaco-editor/react";

import Progress, { keyHandler } from "./Progress";
import Editor from "./Editor";
import { Error, i3Stream, Stream } from "./slangWrapper";

import "./Stacks.css";
import { SubViewProps } from "./App";

type Code = [number, string][];
type CodePointer = number;
type EnvStack = string[];
type Memory = string[];

type Steps = [CodePointer, EnvStack, Memory][];
export type StreamWrapper = {
  code: Code;
  stepStream: Stream<Steps>;
};

const ErrorWrapper = (props: SubViewProps) => {
  const { source } = props;
  const [streamWrapper, setStreamWrapper] = useState<StreamWrapper | Error>(
    i3Stream(source)
  );

  useEffect(() => setStreamWrapper(i3Stream(source)), [source]);

  if ("error" in streamWrapper) {
    return <div className="error">{streamWrapper.error}</div>;
  }

  return (
    <Interpreter3
      {...props}
      streamWrapper={streamWrapper}
      setStreamWrapper={setStreamWrapper}
    />
  );
};

interface I3Props extends SubViewProps {
  streamWrapper: StreamWrapper;
  setStreamWrapper: (s: StreamWrapper) => void;
}

const Interpreter3 = ({
  source,
  onMouseMove,
  onMouseLeave,
  decorations,
  streamWrapper,
  setStreamWrapper,
}: I3Props) => {
  const {
    code,
    stepStream: { steps, next },
  } = streamWrapper;

  const [step, setStep] = useState(0);
  const [currentInst, envStack, memory] = steps[step];
  const cleanCode = code.map(([_, s]) => s).join("\n");

  const envStackS = envStack.join("\n");
  const memoryS = memory.join("\n");

  const showMem = steps.some(([_, __, s]) => s.length > 0);
  const handler = keyHandler(step, setStep, steps.length);

  useEffect(() => {
    setStep(0);
  }, [source]);

  useEffect(() => {
    if (
      step === steps.length - 1 &&
      cleanCode.split("\n")[currentInst] !== "HALT"
    )
      setStreamWrapper({
        code: code,
        stepStream: next(),
      });
  }, [
    step,
    code,
    next,
    currentInst,
    cleanCode,
    steps.length,
    setStreamWrapper,
  ]);

  const decorationsHandler = (e: any, m: Monaco) => {
    return [
      {
        range: new m.Range(currentInst + 1, 1, currentInst + 1, 1),
        options: {
          isWholeLine: true,
          linesDecorationsClassName: "currentLineDec",
        },
      },
      ...decorations(code)(e, m),
    ];
  };

  return (
    <div className="interpreter">
      <div className="interpreterTitle">
        <h3>
          Step {step} - {}
        </h3>
      </div>
      <div className="interpreterEditors">
        <Editor
          value={cleanCode}
          language="javascript"
          onKeyDown={(e) => handler(e.key)}
          decorations={decorationsHandler}
          onMouseMove={onMouseMove(code)}
          onMouseLeave={onMouseLeave}
          options={{
            readOnly: true,
            lineNumbers: (lineNumber: number) => (lineNumber - 1).toString(),
            theme: "vs-dark",
          }}
        />
        <Editor
          value={envStackS}
          language="javascript"
          onKeyDown={(e) => handler(e.key)}
          options={{
            readOnly: true,
            lineNumbers: (lineNumber: number) =>
              envStackS.split("\n").length - lineNumber + 1,
            minimap: { enabled: false },
          }}
        />
        {showMem ? (
          <Editor
            value={memoryS}
            language="javascript"
            onKeyDown={(e) => handler(e.key)}
            options={{
              readOnly: true,
              lineNumbers: (lineNumber: number) => (lineNumber - 1).toString(),
              theme: "vs-dark",
              minimap: { enabled: false },
            }}
          />
        ) : null}
      </div>
      <Progress values={steps} index={step} setIndex={setStep} />
    </div>
  );
};

export default ErrorWrapper;
