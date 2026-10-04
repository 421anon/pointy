(function () {
  const SIDEBAR = ".sidebar";
  const BODY = ".sidebar-body";

  const observers = new WeakMap();
  let wired = null;

  function notify() {
    document.querySelector(BODY)?.dispatchEvent(new Event("scroll"));
  }

  function wire(sidebar) {
    if (observers.has(sidebar)) return;
    const resize = new ResizeObserver(notify);
    const mutations = new MutationObserver(notify);
    resize.observe(sidebar);
    mutations.observe(sidebar, {
      attributes: true,
      childList: true,
      subtree: true,
      characterData: true,
    });
    observers.set(sidebar, [resize, mutations]);
    wired = sidebar;
    notify();
  }

  function sync() {
    if (wired?.isConnected) return;
    const sidebar = document.querySelector(SIDEBAR);
    if (sidebar) wire(sidebar);
  }

  new MutationObserver(sync).observe(document.body, {
    childList: true,
    subtree: true,
  });
  sync();
})();
