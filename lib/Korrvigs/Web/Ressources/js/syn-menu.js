function cleanSynMenu(ev, menu, cleaner) {
  document.body.removeEventListener("click", (ev) => cleanSynMenu(ev, menu, cleaner));
  cleaner();
}

function setupSynMenu(buttonId, runUrl) {
  const elem = document.getElementById(buttonId);
  elem.addEventListener("click", (ev) => {
    let menu = document.createElement("div");
    menu.id = "codecontextmenu";
    menu.classList.add("contextmenu");

    if(runUrl) {
      let run = document.createElement("p");
      run.innerText = "Update";
      run.onclick = () => fetch(runUrl, {}).then(function(response) {
        window.location.reload();
      });
      menu.appendChild(run);
    }

    const popperInstance = Popper.createPopper(elem, menu, {
      modifiers: [
        { name: "preventOverflow",
          options: {}
        }
      ]
    });
    if(setupSynMenu.menu) {
      setupSynMenu.menu.remove();
    }
    setupSynMenu.menu = menu;
    const unregisterMenu = function () {
      setupSynMenu.menu = null;
      menu.remove();
    }
    menu.onmouseleave = () => unregisterMenu();
    document.body.appendChild(menu);
    setTimeout(() => document.body.addEventListener("click", (ev) => cleanSynMenu(ev, menu, unregisterMenu), false), 10);
  })
}
