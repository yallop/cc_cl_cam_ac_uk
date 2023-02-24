import { useState, useEffect } from "react";
import { useDebounce } from "use-debounce";
import { useMonaco } from "@monaco-editor/react";

import languageDef from "./LanguageDef";
import samplePrograms from "./SamplePrograms";
import Interpreter2 from "./Interpreter2";
import Interpreter3 from "./Interpreter3";
import InterpreterJargon from "./InterpreterJargon";
import "./App.css";
import { code, highlightRowsForLocation } from "./slangWrapper";
import Editor from "./Editor";
import IntermediateSteps from "./IntermediateSteps";

const { fib } = samplePrograms;

type sourceHighlight = {
  highlight: boolean;
  line: number;
};

export interface SubViewProps {
  source: string;
  onMouseMove: (code: code) => (event: any) => void;
  onMouseLeave: () => void;
  decorations: (code: code) => (
    e: any,
    m: any
  ) => {
    range: any;
    options: {
      isWholeLine: boolean;
      linesDecorationsClassName: string;
    };
  }[];
}

const subViews: {
  [key: string]: [string, (s: SubViewProps) => JSX.Element];
} = {
  allViews: ["All", IntermediateSteps],
  interp2: ["Interpreter 2", Interpreter2],
  interp3: ["Interpreter 3", Interpreter3],
  jargon: ["Jargon", InterpreterJargon],
};

function App() {
  const [volatileSource, setSource] = useState(fib);
  const [source] = useDebounce(volatileSource, 1000);

  const [subView, setSubView] = useState("jargon");
  const SubViewElement = subViews[subView][1];

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
        linesDecorationsClassName: "sourceLineDec",
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
          linesDecorationsClassName: "sourceLineDec",
        },
      },
    ];
  };

  const monaco = useMonaco();

  useEffect(() => {
    monaco?.editor.setTheme("vs-dark");
    monaco?.languages.register({ id: "Slang" });
    monaco?.languages.setMonarchTokensProvider("Slang", languageDef);
  }, [monaco]);

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
        <h4>Source</h4>
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
            scrollBeyondLastLine: false,
          }}
        />
        <div className="resultBox">
          <select value={subView} onChange={(e) => setSubView(e.target.value)}>
            <option value="allViews">{subViews["allViews"][0]}</option>
            <option value="interp2">{subViews["interp2"][0]}</option>
            <option value="interp3">{subViews["interp3"][0]}</option>
            <option value="jargon">{subViews["jargon"][0]}</option>
          </select>
        </div>
      </div>
      <SubViewElement
        source={source}
        onMouseMove={onMouseMove}
        onMouseLeave={onMouseLeave}
        decorations={decorationsTargetHandler}
      />
    </div>
  );
}

export default App;
