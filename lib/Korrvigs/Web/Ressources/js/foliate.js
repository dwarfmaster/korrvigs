import './view.js'
import { makeBook } from './view.js'

const fetchFile = async url => {
    const res = await fetch(url)
    if (!res.ok) throw new ResponseError(
        `${res.status} ${res.statusText}`, { cause: res })
    return new File([await res.blob()], new URL(res.url).pathname, {
      type: res.headers.get('Content-Type'),
    })
}

async function setupFoliateOn(elem, url) {
  const container = elem.parentElement;
  const buttonPrev = container.querySelector('.foliate-prev-button');
  const buttonNext = container.querySelector('.foliate-next-button');

  const view = document.createElement('foliate-view');
  elem.replaceWith(view);
  const file = await fetchFile(url);
  const book = await makeBook(file);
  await view.open(book);
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
