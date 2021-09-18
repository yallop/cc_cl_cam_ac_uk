const LandingPage = () => (
  <div className="landing">
    <h1>Slang Compiler Explorer</h1>
    <p>
      This online tool demonstrates the compilation stages taught in the Part II
      compilers course. It also demonstrates how the virtual machines for each
      of the stages execute their representations.
    </p>
    <p>
      Edit the source on the left to get started or choose an interpreter to
      visualize its execution execution. Choose the interpreter using the select
      on the bottom left.
    </p>
    <p>
      Use the arrow keys to navigate evaluation steps (as long as the editor on
      the left is not in focus).
    </p>
    <p>Programs are encoded in the url so they can easily be shared.</p>
  </div>
);

export default LandingPage;
