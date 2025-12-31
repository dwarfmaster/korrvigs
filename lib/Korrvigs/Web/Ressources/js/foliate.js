import './view.js'

async function setupFoliateOn(elem, url) {
  const container = elem.parentElement;
  const buttonPrev = container.querySelector('.foliate-prev-button');
  const buttonNext = container.querySelector('.foliate-next-button');

  const view = document.createElement('foliate-view');
  elem.replaceWith(view);
  await view.open(url);
  view.renderer.next();

  buttonPrev.addEventListener('click', e => view.prev());
  buttonNext.addEventListener('click', e => view.next());
}

function initFoliate() {
  document.querySelectorAll('[foliate-url]').forEach((elem) => {
    setupFoliateOn(elem, elem.getAttribute("foliate-url"));
  });
}

if(document.readyState !== 'loading') {
  initFoliate();
} else {
  document.addEventListener("DOMContentLoaded", initFoliate);
}
