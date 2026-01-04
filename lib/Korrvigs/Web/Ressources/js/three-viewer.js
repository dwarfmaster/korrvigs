const {ThreeViewer, SSAOPlugin, PhysicalMaterial, Color} = threepipe

async function setupThreeViewer(elem, url, hdrUrl) {
  const viewer = new ThreeViewer({canvas: elem});
  viewer.addPluginSync(new SSAOPlugin());
  await viewer.setEnvironmentMap(hdrUrl, {
    setBackground: true,
  })
  await viewer.load(url, {
    autoCenter: true,
    autoScale: true
  });
}

function initThreeViewer () {
  document.querySelectorAll('[three-data]').forEach((elem) => {
    setupThreeViewer(elem, elem.getAttribute("three-data"), elem.getAttribute("three-hdr"));
  });
}

if(document.readyState !== 'loading') {
  initThreeViewer();
} else {
  document.addEventListener("DOMContentLoaded", initThreeViewer);
}
