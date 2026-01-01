function cleanHdMenu(ev, menu, cleaner) {
  document.body.removeEventListener("click", (ev) => cleanHdMenu(ev, menu, cleaner));
  cleaner();
}

function sendHeaderAction(actUrl, action) {
  fetch(actUrl, {
    method: "POST",
    body: action
  }).then((response) => response.text()).then((url) => {
    window.location = url;
  });
}

function setupHeaderMenu(buttonId, editFn, openUrl, actUrl) {
  const elem = document.getElementById(buttonId);
  if(!editFn && !openUrl && !actUrl) {
    elem.remove();
    return;
  }

  elem.addEventListener("click", (ev) => {
    let menu = document.createElement("div");
    menu.id = "hdcontextmenu";
    menu.classList.add("contextmenu");

    if(editFn) {
      let edit = document.createElement("p");
      edit.innerText = "Edit";
      editFn(edit);
      menu.appendChild(edit);
    }

    if(openUrl) {
      let open = document.createElement("p");
      let openLink = document.createElement("a");
      openLink.href = openUrl;
      openLink.innerText = "Open";
      open.appendChild(openLink);
      menu.appendChild(open);
    }

    if(actUrl) {
      const createSub = (text, act) => {
        let elem = document.createElement("p");
        elem.innerText = text;
        elem.addEventListener("click", () => sendHeaderAction(actUrl, act));
        menu.appendChild(elem);
      };
      createSub("New first sub", "sub-first");
      createSub("New last sub", "sub-last");
      createSub("Header after", "header-after");
      createSub("Header before", "header-before");
    }

    const popperInstance = Popper.createPopper(elem, menu, {
      modifiers: [
        { name: "preventOverflow",
          options: {}
        }
      ]
    });
    if(setupHeaderMenu.menu) {
      setupHeaderMenu.menu.remove();
    }
    setupHeaderMenu.menu = menu;
    const unregisterMenu = function () {
      setupHeaderMenu.menu = null;
      menu.remove();
    }
    menu.onmouseleave = () => unregisterMenu();
    document.body.appendChild(menu);
    setTimeout(() => document.body.addEventListener("click", (ev) => cleanHdMenu(ev, menu, unregisterMenu), false), 10);
  });
}
