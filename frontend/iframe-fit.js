(function () {
  const FIT_FRAME = ".iframe-zoom-wrapper-fit > iframe";

  const observers = new WeakMap();

  function fit(frame) {
    const doc = frame.contentDocument;
    if (!doc || !doc.body) return;
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
