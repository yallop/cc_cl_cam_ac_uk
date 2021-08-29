import { useState, useEffect } from "react";
import { useDebounce } from "use-debounce";
import { useMonaco } from "@monaco-editor/react";

import languageDef from "./LanguageDef";
import samplePrograms from "./SamplePrograms";
import { useKeypress } from "./util";
import Interpreter2 from "./Interpreter2";
import Interpreter3 from "./Interpreter3";
import InterpreterJargon from "./InterpreterJargon";
import "./App.css";
import {
  i2compile,
  computeI2steps,
  i3compile,
  computeI3steps,
  interp,
  jargonCompile,
  computeJargonSteps,
  stringOfCode,
  code,
  highlightRowsForLocation,
} from "./slangWrapper";
import Editor from "./Editor";

const { fib } = samplePrograms;
// TODO: fix slang crash bug
// //@ts-ignore
// for (const property in slang) {
//   //@ts-ignore
//   slang[property] = (...args: any[]) => {
//     try {
//       //@ts-ignore
//       return slang[property](...args)
//     } catch {
//       return "\"stack overflow\""
//     }
//   }
// }

type sourceHighlight = {
  highlight: boolean;
  line: number;
};

function App() {
  const [volatileSource, setSource] = useState(fib);
  const [source] = useDebounce(volatileSource, 1000);

  const [result, setResult] = useState(interp(source));

  const [i2code, setI2code] = useState(i2compile(source));
  const i2codeString = stringOfCode(i2code).slice(1, -1);
  const [i3code, setI3code] = useState(i3compile(source));
  const i3codeString = stringOfCode(i3code);
  const [jargonCode, setJargonCode] = useState(jargonCompile(source));
  const jargonCodeString = stringOfCode(jargonCode);

  const [i2Steps, setI2Steps] = useState(computeI2steps(source));
  const [i3Steps, setI3Steps] = useState(computeI3steps(source));
  const [jargonSteps, setJargonSteps] = useState(computeJargonSteps(source));

  const [showI2, setShowI2] = useState(false);
  const [showI3, setShowI3] = useState(false);
  const [showJargon, setShowJargon] = useState(false);

  const [volatileSourceHighlight, setSourceHighlight] =
    useState<sourceHighlight>({
      highlight: false,
      line: 0,
    });
  const [sourceHighlight] = useDebounce(volatileSourceHighlight, 50);

  const decorationsTargetHandler = (code: code) => (e: any, m: any) => {
    if (!sourceHighlight.highlight) return [];

    const linesToHighlight = highlightRowsForLocation(
      code,
      sourceHighlight.line
    );

    return linesToHighlight.map((l) => ({
      range: new m.Range(l + 1, 1, l + 1, 1),
      options: {
        isWholeLine: true,
        linesDecorationsClassName: "currentLineDec",
      },
    }));
  };

  const decorationsSourceHandler = (e: any, m: any) => {
    if (!sourceHighlight.highlight) return [];

    return [
      {
        range: new m.Range(sourceHighlight.line, 1, sourceHighlight.line, 1),
        options: {
          isWholeLine: true,
          linesDecorationsClassName: "currentLineDec",
        },
      },
    ];
  };

  useKeypress(["Escape"], () => {
    setShowI2(false);
    setShowI3(false);
    setShowJargon(false);
  });

  const monaco = useMonaco();

  useEffect(() => {
    monaco?.editor.setTheme("vs-dark");
    monaco?.languages.register({ id: "Slang" });
    monaco?.languages.setMonarchTokensProvider("Slang", languageDef);
  }, [monaco]);

  useEffect(() => {
    setResult(interp(source));
    setI2code(i2compile(source));
    setI3code(i3compile(source));
    setI2Steps(computeI2steps(source));
    setI3Steps(computeI3steps(source));
    setJargonSteps(computeJargonSteps(source));
    setJargonCode(jargonCompile(source));
  }, [source]);

  const onMouseMove = (code: code) => (event: any) => {
    const lineNumber = event?.target?.position?.lineNumber;
    if (lineNumber !== null && lineNumber !== undefined)
      setSourceHighlight({ line: code[lineNumber - 1][0], highlight: true });
  };
  const onMouseMoveSource = (event: any) => {
    const lineNumber = event?.target?.position?.lineNumber;
    if (lineNumber !== null && lineNumber !== undefined)
      setSourceHighlight({ line: lineNumber, highlight: true });
  };
  const onMouseLeave = () => setSourceHighlight({ highlight: false, line: 0 });

  return (
    <div className="App">
      <div className="editorWrapper">
        <h4>Slang</h4>
        <Editor
          height="86vh"
          defaultValue={source}
          defaultLanguage="Slang"
          onMouseMove={onMouseMoveSource}
          onMouseLeave={onMouseLeave}
          decorations={decorationsSourceHandler}
          onChange={(value, _) =>
            value === undefined ? setSource("") : setSource(value)
          }
          options={{
            theme: "vs-dark",
            minimap: { enabled: false },
          }}
        />
        <div className="resultBox">
          <p>Result: {result}</p>
        </div>
      </div>
      <div className="editorWrapper">
        <h4>Interpreter 2</h4>
        <Editor
          defaultLanguage="javascript"
          height="86vh"
          value={i2codeString}
          onMouseMove={onMouseMove(i2code)}
          onMouseLeave={onMouseLeave}
          decorations={decorationsTargetHandler(i2code)}
          options={{
            tabSize: 2,
            readOnly: true,
            theme: "vs-dark",
            minimap: { enabled: false },
          }}
        />
        <button onClick={() => setShowI2(!showI2)}>
          Visualize Interpretation
        </button>
      </div>
      <div className="editorWrapper">
        <h4>Interpreter 3</h4>
        <Editor
          defaultLanguage="javascript"
          value={i3codeString}
          onMouseMove={onMouseMove(i3code)}
          onMouseLeave={onMouseLeave}
          decorations={decorationsTargetHandler(i3code)}
          height="86vh"
          options={{
            readOnly: true,
            theme: "vs-dark",
            minimap: { enabled: false },
          }}
        />
        <button onClick={() => setShowI3(!showI3)}>
          Visualize Interpretation
        </button>
      </div>
      <div className="editorWrapper">
        <h4>Jargon</h4>
        <Editor
          defaultLanguage="javascript"
          value={jargonCodeString}
          onMouseMove={onMouseMove(jargonCode)}
          onMouseLeave={onMouseLeave}
          decorations={decorationsTargetHandler(jargonCode)}
          height="86vh"
          options={{
            readOnly: true,
            theme: "vs-dark",
            minimap: { enabled: false },
          }}
        />
        <button onClick={() => setShowJargon(!showJargon)}>
          Visualize Interpretation
        </button>
      </div>
      {showI2 ? (
        <Interpreter2 steps={i2Steps} onClose={() => setShowI2(false)} />
      ) : null}
      {showI3 ? (
        <Interpreter3 steps={i3Steps} onClose={() => setShowI3(false)} />
      ) : null}
      {showJargon ? (
        <InterpreterJargon
          steps={jargonSteps}
          onClose={() => setShowJargon(false)}
        />
      ) : null}
    </div>
  );
}

export default App;
