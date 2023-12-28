import { useEffect, useState } from "react";
import CytoscapeComponent from "react-cytoscapejs";
import Cytoscape from "cytoscape";
//@ts-ignore
import klay from "cytoscape-klay";
import { useDebouncedCallback } from "use-debounce";

import Progress, { keyHandler } from "./Progress";
import Editor from "./Editor";
import "./Stacks.css";
import { jargonStream, Stream } from "./slangWrapper";

Cytoscape.use(klay);

export type Step = {
  stack: string[];
  heap: string[];
  heap_graph: heap_graph;
  cp: number;
  fp: number;
  sp: number;
  status: string;
}[];

export type StreamWrapper = {
  code: string[];
  stepStream: Stream<Step>;
};

type node_type = "H_INT" | "H_BOOL" | "H_CI" | "H_HI" | "H_HEADER";
type heap_node = {
  id: string;
  label: string;
  tp: [node_type];
  pointer: number;
};

type edge_type = "BLOCK" | "POINTER";
type heap_edge = {
  source: string;
  target: string;
  label: string;
  tp: [edge_type];
};

type heap_graph = [heap_node[], heap_edge[]];

type pointers = {
  heap: {
    show: boolean;
    val: number;
  };
  code: {
    showHeap: boolean;
    heap: number;
  };
};

const InterpreterJargon = ({
  source,
  onClose,
}: {
  source: string;
  onClose?: () => void;
}) => {
  const [
    {
      code,
      stepStream: { steps, next },
    },
    setStream,
  ] = useState(jargonStream(source));

  const codeString = code.join("\n");
  const [step, setStep] = useState(0);
  const { stack, cp, fp, heap, heap_graph } = steps[step];

  const env = stack
    .slice()
    .reverse()
    .map((s) => s.slice(6))
    .join("\n");

  const currentInst = cp + 1;
  const currentFrame = stack.length - fp;

  const memory = heap.join("\n");
  const showMemory = steps.some(({ heap }) => heap.length > 0);

  const handler = keyHandler(step, setStep, steps.length);

  useEffect(() => {
    setStream(jargonStream(source));
  }, [source]);

  useEffect(() => {
    if (step === steps.length - 1 && code[cp].trim() !== "HALT") {
      setStream({ code, stepStream: next() });
    }
  }, [code, cp, next, step, steps.length]);

  const [pointers, setPointers] = useState<pointers>({
    heap: {
      show: false,
      val: 0,
    },
    code: {
      showHeap: false,
      heap: 0,
    },
  });

  const codeDecorationsHandler = (e: any, m: any) => {
    e.revealRange(new m.Range(currentInst, 1, currentInst, 1));
    e.setPosition({
      column: 0,
      lineNumber: currentInst,
    });
    return [
      {
        range: new m.Range(currentInst, 1, currentInst, 1),
        options: {
          isWholeLine: true,
          linesDecorationsClassName: "currentLineDec",
        },
      },
      {
        range: new m.Range(pointers.code.heap, 1, pointers.code.heap, 1),
        options: {
          isWholeLine: true,
          linesDecorationsClassName: pointers.code.showHeap
            ? "heapPointerDec"
            : "",
        },
      },
    ];
  };

  const envDecorationsHandler = (e: any, m: any) => {
    return [
      {
        range: new m.Range(currentFrame, 1, currentFrame, 1),
        options: {
          isWholeLine: true,
          linesDecorationsClassName: "framePointerDec",
        },
      },
    ];
  };

  const heapDecorationsHandler = (e: any, m: any) => {
    if (pointers.heap.show) {
      e.revealRange(new m.Range(pointers.heap.val, 1, pointers.heap.val, 1));
    }
    return [
      {
        range: new m.Range(pointers.heap.val, 1, pointers.heap.val, 1),
        options: {
          isWholeLine: true,
          linesDecorationsClassName: pointers.heap.show ? "heapPointerDec" : "",
        },
      },
    ];
  };

  const [nodes, edges] = heap_graph;

  const elements = [
    ...nodes.map((v) => ({
      data: v,
      style: nodeStyle(v.tp[0]),
    })),
    ...edges.map((v) => ({
      data: v,
    })),
  ];

  const onMouseNodeOver = useDebouncedCallback((e: any) => {
    const data = e.target._private.data;
    const isCI = data.tp[0] === "H_CI";
    const pointer = {
      heap: {
        val: parseInt(data.id) + 1,
        show: true,
      },
      code: {
        showHeap: isCI,
        heap: isCI ? data.pointer + 1 : pointers.code.heap,
      },
    };

    setPointers(pointer);
  }, 50);

  const onMouseNodeOff = useDebouncedCallback((e: any) => {
    setPointers({
      heap: {
        val: pointers.heap.val,
        show: false,
      },
      code: {
        showHeap: false,
        heap: pointers.code.heap,
      },
    });
  }, 50);

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
          value={codeString}
          width="33%"
          language="javascript"
          onKeyDown={(e) => handler(e.key)}
          decorations={codeDecorationsHandler}
          options={{
            readOnly: true,
            lineNumbers: (lineNumber: number) => (lineNumber - 1).toString(),
            theme: "vs-dark",
          }}
        />
        <Editor
          value={env}
          language="javascript"
          width="33%"
          onKeyDown={(e) => handler(e.key)}
          decorations={envDecorationsHandler}
          options={{
            readOnly: true,
            lineNumbers: (lineNumber: number) =>
              (env.split("\n").length - lineNumber + 1).toString(),
            minimap: { enabled: false },
          }}
        />
        <div className="jargonMem">
          {showMemory ? (
            <Editor
              value={memory}
              height="50%"
              decorations={heapDecorationsHandler}
              language="javascript"
              onKeyDown={(e) => handler(e.key)}
              options={{
                readOnly: true,
                lineNumbers: (lineNumber: number) =>
                  (lineNumber - 1).toString(),
                theme: "vs-dark",
                minimap: { enabled: false },
              }}
            />
          ) : null}

          <CytoscapeComponent
            id="heap"
            elements={elements}
            style={{ height: "50%" }}
            cy={(cy) =>
              cy.on("add", "node", (_evt) => {
                cy.layout({
                  name: "klay",
                  //@ts-ignore
                  klay: {
                    direction: "DOWN",
                    borderSpacing: 80,
                    spacing: 80,
                  },
                }).run();
                cy.fit();
                cy.on("mouseover", "node", onMouseNodeOver);
                cy.on("mouseout", "node", onMouseNodeOff);
              })
            }
          />
        </div>
      </div>
      <Progress values={steps} index={step} setIndex={setStep} />
    </div>
  );
};

function nodeStyle(nodeTp: node_type) {
  let style;
  switch (nodeTp) {
    case "H_INT":
      style = {
        shape: "square",
        "background-color": "white",
      };
      break;
    case "H_BOOL":
      style = {
        shape: "barrel",
        "background-color": "purple",
      };
      break;
    case "H_CI":
      style = {
        shape: "tag",
        "background-color": "red",
      };
      break;
    case "H_HI":
      style = {
        shape: "round-tag",
        "background-color": "white",
      };
      break;
    case "H_HEADER":
      style = {
        "background-color": "black",
      };
  }

  style = {
    ...style,
    "text-margin-y": "-10",
    "text-justification": "left",
    "text-wrap": "wrap",
    color: "green",
  };
  return style;
}

export default InterpreterJargon;
