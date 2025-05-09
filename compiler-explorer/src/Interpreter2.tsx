import { useEffect, useState } from "react";

import Editor from "./Editor";
import Progress, { keyHandler } from "./Progress";
import { computeI2steps } from "./slangWrapper";

import "./Stacks.css";

type CodeStack = string[];
type EnvStack = string[];
type Memory = string[];

type Steps = [CodeStack, EnvStack, Memory][];

const trimClosures = (ss: string[]) => {
  return ss.map((s, i) =>
    s.length > 40 ? s.split("\n").join(" ").slice(0, 40) + "..." : s
  );
};

const Interpreter2 = ({
  source,
  onClose,
}: {
  source: string;
  onClose?: () => void;
}) => {
  const [steps, setSteps] = useState<Steps>(computeI2steps(source));
  // We only compile the steps if the prop changes
  useEffect(() => {
    setSteps(computeI2steps(source));
  }, [source]);

  const [step, setStep] = useState(0);
  const [codeStack, envStack, memory] = steps[step];

  const [showNestedInstructions, setShowNestedInstructions] = useState(false);

  const codeStackS = (
    showNestedInstructions ? codeStack : trimClosures(codeStack)
  ).join("\n");
  const envStackS = (
    showNestedInstructions ? envStack : trimClosures(envStack)
  ).join("\n");
  const memoryS = memory.join("\n");

  const showMem = steps.some(([_, __, s]) => s.length > 0);

  const handler = keyHandler(step, setStep, steps.length);

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
        {onClose ? <button onClick={onClose}>X</button> : null}
      </div>
      <div className="interpreterEditors">
        <Editor
          value={codeStackS}
          language="javascript"
          onKeyDown={(e) => handler(e.key)}
          theme="vs-dark"
          options={{
            readOnly: true,
            lineNumbers: (lineNumber: number) =>
              codeStackS.split("\n").length - lineNumber + 1,
          }}
        />
        <Editor
          value={envStackS}
          className="i2StackEditor"
          language="javascript"
          onKeyDown={(e) => handler(e.key)}
          theme="vs-dark"
          options={{
            readOnly: true,
            lineNumbers: (lineNumber: number) =>
              envStackS.split("\n").length - lineNumber + 1,
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
            }}
          />
        ) : null}
      </div>
      <Progress values={steps} index={step} setIndex={setStep} />
    </div>
  );
};

export default Interpreter2;
