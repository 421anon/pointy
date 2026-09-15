// Frames that take their height from the document they show: the review diff is
// served as a page of its own, so it cannot know how tall the row around it is.
// The parent measures it, which it can: the report is same-origin.
(function () {
  const FIT_FRAME = ".iframe-zoom-wrapper-fit > iframe";

  const observers = new WeakMap();

  function fit(frame) {
    const doc = frame.contentDocument;
    if (!doc || !doc.body) return;
    // The body, not the document element: a document shorter than the frame
    // still reports the frame's height, which is what this is here to undo.
    // Rounded up, so a sub-pixel of content never becomes a scrollbar.
    const height = Math.ceil(doc.body.getBoundingClientRect().height);
    if (height > 0 && frame.style.height !== `${height}px`) {
      frame.style.height = `${height}px`;
    }
  }

  function watch(frame) {
    fit(frame);
    observers.get(frame)?.disconnect();
    const body = frame.contentDocument?.body;
    if (!body) return;
    // Zooming the report, and the rewrapping that comes with a narrower row,
    // change how tall it is.
    const observer = new ResizeObserver(() => fit(frame));
    observer.observe(body);
    observers.set(frame, observer);
  }

  document.addEventListener(
    "load",
    (event) => {
      const frame = event.target;
      if (frame instanceof HTMLIFrameElement && frame.matches(FIT_FRAME)) {
        watch(frame);
      }
    },
    true
  );
})();
