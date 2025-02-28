import { useEffect, useState } from "react";

import Progress, { keyHandler } from "./Progress";
import Editor from "./Editor";
import { computeI3steps } from "./slangWrapper";

import "./Stacks.css";
import { Monaco } from "@monaco-editor/react";

type Code = string;
type CodePointer = number;
type EnvStack = string[];
type Memory = string[];
type Steps = [Code, [CodePointer, EnvStack, Memory][]];

const Interpreter3 = ({
  source,
  onClose,
}: {
  source: string;
  onClose?: () => void;
}) => {
  const [steps, setSteps] = useState<Steps>(computeI3steps(source));
  // We only compile the steps if the prop changes
  useEffect(() => {
    setSteps(computeI3steps(source));
  }, [source]);

  let [installedCode, stepList] = steps;

  installedCode = clean(installedCode);

  const [step, setStep] = useState(0);

  const [currentInst, envStack, memory] = stepList[step];
  const envStackS = envStack.join("\n");
  const memoryS = memory.join("\n");
  const showMem = stepList.some(([_, __, s]) => s.length > 0);

  const decorationsHandler = (e: any, m: Monaco) => {
    e.revealRange(new m.Range(currentInst, 1, currentInst, 1));
    return [
      {
        range: new m.Range(currentInst + 1, 1, currentInst + 1, 1),
        options: {
          isWholeLine: true,
          linesDecorationsClassName: "currentLineDec",
        },
      },
    ];
  };

  const handler = keyHandler(step, setStep, stepList.length);

  return (
    <div className="interpreter">
      <div className="interpreterTitle">
        <h3>
          Step {step} - {}
        </h3>
        {onClose ? <button onClick={onClose}>X</button> : null}
      </div>
      <div className="interpreterEditors">
        <Editor
          value={installedCode}
          language="javascript"
          onKeyDown={(e) => handler(e.key)}
          decorations={decorationsHandler}
          theme="vs-dark"
          options={{
            readOnly: true,
            lineNumbers: (lineNumber: number) => (lineNumber - 1).toString(),
          }}
        />
        <Editor
          value={envStackS}
          language="javascript"
          onKeyDown={(e) => handler(e.key)}
          theme="vs-dark"
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
            theme="vs-dark"
            options={{
              readOnly: true,
              lineNumbers: (lineNumber: number) => (lineNumber - 1).toString(),
              minimap: { enabled: false },
            }}
          />
        ) : null}
      </div>
      <Progress values={steps[1]} index={step} setIndex={setStep} />
    </div>
  );
};

function clean(code: string): string {
  return code
    .split("\n")
    .map((i) => i.split(" ").slice(1).join(" "))
    .filter((i) => i.length > 0)
    .join("\n");
}

export default Interpreter3;
