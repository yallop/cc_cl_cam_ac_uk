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
  code,
  CompilerResult,
  highlightRowsForLocation,
  i2compile,
  i3compile,
  interp,
  jargonCompile,
  stringOfCode,
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

  const [result, setResult] = useState("");

  const [i2Result, setI2Result] = useState<CompilerResult>(i2compile(source));
  const [i3Result, setI3Result] = useState<CompilerResult>(i3compile(source));
  const [jargonResult, setJargonResult] = useState<CompilerResult>(
    jargonCompile(source)
  );

  const i2codeString =
    i2Result.code && stringOfCode(i2Result.code).slice(1, -1);
  const i3codeString = i3Result.code && stringOfCode(i3Result.code);
  const jargonCodeString = jargonResult.code && stringOfCode(jargonResult.code);

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
    monaco?.languages.register({ id: "Slang" });
    monaco?.languages.setMonarchTokensProvider("Slang", languageDef);
  }, [monaco]);

  useEffect(() => {
    setResult("");
    setI2Result(i2compile(source));
    setI3Result(i3compile(source));
    setJargonResult(jargonCompile(source));
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
          theme="vs-dark"
          options={{
            minimap: { enabled: false },
          }}
        />
        <div className="resultBox">
          <button disabled={!!result} onClick={() => setResult(interp(source))}>
            {result === "" ? "Compute Result" : result}
          </button>
        </div>
      </div>
      <div className="editorWrapper">
        <h4>Interpreter 2</h4>
        {i2Result.error ? (
          <div className="errorBox">{i2Result.error}</div>
        ) : (
          <Editor
            defaultLanguage="javascript"
            height="86vh"
            value={i2codeString}
            onMouseMove={onMouseMove(i2Result.code)}
            onMouseLeave={onMouseLeave}
            decorations={decorationsTargetHandler(i2Result.code)}
            theme="vs-dark"
            options={{
              tabSize: 2,
              readOnly: true,
              minimap: { enabled: false },
            }}
          />
        )}
        <button onClick={() => setShowI2(!showI2)}>
          Visualize Interpretation
        </button>
      </div>
      <div className="editorWrapper">
        <h4>Interpreter 3</h4>
        {i3Result.error ? (
          <div className="errorBox">{i3Result.error}</div>
        ) : (
          <Editor
            defaultLanguage="javascript"
            value={i3codeString}
            onMouseMove={onMouseMove(i3Result.code)}
            onMouseLeave={onMouseLeave}
            decorations={decorationsTargetHandler(i3Result.code)}
            height="86vh"
            theme="vs-dark"
            options={{
              readOnly: true,
              minimap: { enabled: false },
            }}
          />
        )}
        <button onClick={() => setShowI3(!showI3)}>
          Visualize Interpretation
        </button>
      </div>
      <div className="editorWrapper">
        <h4>Jargon</h4>
        {jargonResult.error ? (
          <div className="errorBox">{jargonResult.error}</div>
        ) : (
          <Editor
            defaultLanguage="javascript"
            value={jargonCodeString}
            onMouseMove={onMouseMove(jargonResult.code)}
            onMouseLeave={onMouseLeave}
            decorations={decorationsTargetHandler(jargonResult.code)}
            height="86vh"
            theme="vs-dark"
            options={{
              readOnly: true,
              minimap: { enabled: false },
            }}
          />
        )}
        <button onClick={() => setShowJargon(!showJargon)}>
          Visualize Interpretation
        </button>
      </div>
      {showI2 ? (
        <Interpreter2 source={source} onClose={() => setShowI2(false)} />
      ) : null}
      {showI3 ? (
        <Interpreter3 source={source} onClose={() => setShowI3(false)} />
      ) : null}
      {showJargon ? (
        <InterpreterJargon
          source={source}
          onClose={() => setShowJargon(false)}
        />
      ) : null}
    </div>
  );
}

export default App;
