import { SubViewProps } from "./App";
import Editor from "./Editor";
import {
  i2compile,
  i3compile,
  jargonCompile,
  stringOfCode,
} from "./slangWrapper";

const IntermediateSteps = ({
  source,
  decorations,
  onMouseMove,
  onMouseLeave,
}: SubViewProps) => {
  const i2code = i2compile(source);

  if (i2code[0][1].toLowerCase().startsWith("error")) {
    return <div className="error">{i2code[0][1]}</div>;
  }
  const i2codeString = stringOfCode(i2code).slice(1, -1);

  const i3code = i3compile(source);
  const i3codeString = stringOfCode(i3code);
  const jargonCode = jargonCompile(source);
  const jargonCodeString = stringOfCode(jargonCode);

  return (
    <>
      <div className="editorWrapper">
        <h4>Interpreter 2</h4>
        <Editor
          defaultLanguage="javascript"
          height="86vh"
          value={i2codeString}
          onMouseMove={onMouseMove(i2code)}
          onMouseLeave={onMouseLeave}
          decorations={decorations(i2code)}
          options={{
            tabSize: 2,
            readOnly: true,
            theme: "vs-dark",
            minimap: { enabled: false },
          }}
        />
      </div>
      <div className="editorWrapper">
        <h4>Interpreter 3</h4>
        <Editor
          defaultLanguage="javascript"
          value={i3codeString}
          onMouseMove={onMouseMove(i3code)}
          onMouseLeave={onMouseLeave}
          decorations={decorations(i3code)}
          height="86vh"
          options={{
            readOnly: true,
            theme: "vs-dark",
            minimap: { enabled: false },
          }}
        />
      </div>
      <div className="editorWrapper">
        <h4>Jargon</h4>
        <Editor
          defaultLanguage="javascript"
          value={jargonCodeString}
          onMouseMove={onMouseMove(jargonCode)}
          onMouseLeave={onMouseLeave}
          decorations={decorations(jargonCode)}
          height="86vh"
          options={{
            readOnly: true,
            theme: "vs-dark",
            minimap: { enabled: false },
          }}
        />
      </div>
    </>
  );
};

export default IntermediateSteps;
