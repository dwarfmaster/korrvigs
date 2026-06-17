function cleanSynMenu(ev, menu, cleaner) {
  document.body.removeEventListener("click", (ev) => cleanSynMenu(ev, menu, cleaner));
  cleaner();
}

function setupSynMenu(buttonId, synUrl, redirUrl) {
  const elem = document.getElementById(buttonId);
  elem.addEventListener("click", (ev) => {
    ev.preventDefault();
    ev.stopPropagation();

    let menu = document.createElement("div");
    menu.id = "codecontextmenu";
    menu.classList.add("contextmenu");

    let run = document.createElement("p");
    run.innerText = "Update";
    run.onclick = () => fetch(synUrl, {method: "POST"}).then(function(response) {
      window.location = redirUrl;
    });
    menu.appendChild(run);

    let view = document.createElement("p");
    view.innerText = "View";
    view.onclick = () => fetch(synUrl).then((response) => response.text()).then(function(text) {
      let lines = text.split(/\n/);
      console.log(lines);
      // TODO
    });
    menu.appendChild(view);

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
