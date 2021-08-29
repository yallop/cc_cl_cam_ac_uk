import EditorInternal, { EditorProps, Monaco } from "@monaco-editor/react";
import { useEffect, useRef } from "react";
import { useState } from "react";

interface IEditorProps extends EditorProps {
  onKeyDown?: (e: KeyboardEvent) => void;
  decorations?: (editor: any, monaco: Monaco) => any;
  onMouseMove?: (e: any) => void;
  onMouseLeave?: (e: any) => void;
}

const Editor = (props: IEditorProps) => {
  const setDecorations = useState({})[1];
  const { decorations } = props;

  const editorRef = useRef<any>();
  const monacoRef = useRef<any>();

  const onMount = (e: any, m: any) => {
    editorRef.current = e;
    monacoRef.current = m;
    if (decorations) {
      setDecorations((old_dec) =>
        editorRef.current?.deltaDecorations(
          old_dec,
          decorations(editorRef.current, monacoRef.current)
        )
      );
    }
  };

  const onMouseMoveRef = useRef(props.onMouseMove);
  onMouseMoveRef.current = props.onMouseMove;

  const onMouseLeaveRef = useRef(props.onMouseLeave);
  onMouseLeaveRef.current = props.onMouseLeave;

  const keyDownRef = useRef(props.onKeyDown);
  useEffect(() => {
    keyDownRef.current = props.onKeyDown;
  }, [props.onKeyDown]);

  useEffect(() => {
    if (decorations && editorRef.current && monacoRef.current)
      setDecorations((old_dec) =>
        editorRef.current?.deltaDecorations(
          old_dec,
          decorations(editorRef.current, monacoRef.current)
        )
      );
  }, [decorations, setDecorations]);

  return (
    <EditorInternal
      {...props}
      onMount={(e, m) => {
        e.onMouseMove((e: any) => {
          if (onMouseMoveRef.current) onMouseMoveRef.current(e);
        });
        e.onMouseLeave((e: any) => {
          if (onMouseLeaveRef.current) onMouseLeaveRef.current(e);
        });
        if (props.onMount) props.onMount(e, m);
        onMount(e, m);
        e.onKeyDown(({ browserEvent }: { browserEvent: KeyboardEvent }) => {
          if (keyDownRef.current) keyDownRef.current(browserEvent);
        });
      }}
    />
  );
};

export default Editor;
