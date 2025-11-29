function cleanCodeMenu(ev, menu, cleaner) {
  document.body.removeEventListener("click", (ev) => cleanCodeMenu(ev, menu, cleaner));
  cleaner();
}

function setupCodeMenu(buttonId, editFn, codeUrl, computeUrl) {
  const elem = document.getElementById(buttonId);
  elem.addEventListener("click", (ev) => {
    let menu = document.createElement("div");
    menu.id = "codecontextmenu";
    menu.classList.add("contextmenu");

    let edit = document.createElement("p");
    edit.innerText = "Edit";
    editFn(edit);
    menu.appendChild(edit);

    if(codeUrl) {
      let code = document.createElement("p");
      let codeLink = document.createElement("a");
      codeLink.href = codeUrl;
      codeLink.innerText = "Open";
      code.appendChild(codeLink);
      menu.appendChild(code);
    }

    if(computeUrl) {
      let run = document.createElement("p");
      run.innerText = "Run";
      run.onclick = () => fetch(computeUrl).then(function(response) {
        window.location.reload();
      });
      menu.appendChild(run);

      let runForce = document.createElement("p");
      runForce.innerText = "Run force";
      runForce.onclick = () => fetch(computeUrl, {method: "POST"}).then(function(response) {
        window.location.reload();
      });
      menu.appendChild(runForce);
    }

    const popperInstance = Popper.createPopper(elem, menu, {
      modifiers: [
        { name: "preventOverflow",
          options: {}
        }
      ]
    });
    if(setupCodeMenu.menu) {
      setupCodeMenu.menu.remove();
    }
    setupCodeMenu.menu = menu;
    const unregisterMenu = function () {
      setupCodeMenu.menu = null;
      menu.remove();
    }
    menu.onmouseleave = () => unregisterMenu();
    document.body.appendChild(menu);
    setTimeout(() => document.body.addEventListener("click", (ev) => cleanCodeMenu(ev, menu, unregisterMenu), false), 10);
  })
}
