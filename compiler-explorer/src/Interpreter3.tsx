import { useEffect, useState } from "react";

import { Monaco } from "@monaco-editor/react";

import Progress, { keyHandler } from "./Progress";
import Editor from "./Editor";
import { i3Stream, Stream } from "./slangWrapper";

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

const Interpreter3 = ({
  source,
  onMouseMove,
  onMouseLeave,
  decorations,
}: SubViewProps) => {
  const [
    {
      code,
      stepStream: { steps, next },
    },
    setStream,
  ] = useState<StreamWrapper>(i3Stream(source));

  const [step, setStep] = useState(0);
  const [currentInst, envStack, memory] = steps[step];
  const cleanCode = code.map(([_, s]) => s).join("\n");

  const envStackS = envStack.join("\n");
  const memoryS = memory.join("\n");

  const showMem = steps.some(([_, __, s]) => s.length > 0);
  const handler = keyHandler(step, setStep, steps.length);

  useEffect(() => {
    setStream(i3Stream(source));
    setStep(0);
  }, [source]);

  useEffect(() => {
    if (
      step === steps.length - 1 &&
      cleanCode.split("\n")[currentInst] !== "HALT"
    )
      setStream({
        code: code,
        stepStream: next(),
      });
  }, [step, code, next, currentInst, cleanCode, steps.length]);

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

export default Interpreter3;
