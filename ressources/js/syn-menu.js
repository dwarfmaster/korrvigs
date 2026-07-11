function cleanSynMenu(ev, menu, cleaner) {
  document.body.removeEventListener("click", (ev) => cleanSynMenu(ev, menu, cleaner));
  cleaner();
}

function setupSynMenu(buttonId, synUrl, redirUrl, openUrl) {
  const elem = document.getElementById(buttonId);
  elem.addEventListener("click", (ev) => {
    ev.preventDefault();
    ev.stopPropagation();

    let menu = document.createElement("div");
    menu.id = "syncontextmenu";
    menu.classList.add("contextmenu");

    let run = document.createElement("p");
    run.innerText = "Update";
    run.onclick = () => fetch(synUrl, {method: "POST"}).then(function(response) {
      window.location = redirUrl;
    });
    menu.appendChild(run);

    let view = document.createElement("p");
    view.innerText = "View";
    view.onclick = () => fetch(synUrl).then((response) => response.json()).then(function(lines) {
      setupSynContentMenu(elem, lines);
    });
    menu.appendChild(view);

    let open = document.createElement("p");
    let openLink = document.createElement("a");
    openLink.href = openUrl;
    openLink.innerText = "Open";
    open.appendChild(openLink);
    menu.appendChild(open);

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

function cleanSynContentMenu(ev, menu, cleaner) {
  document.body.removeEventListener("click", (ev) => cleanSynContentMenu(ev, menu, cleaner));
  cleaner();
}

function setupSynContentMenu(elem, content) {
  let menu = document.createElement("div");
  menu.id = "syncontentcontextmenu";
  menu.classList.add("contextmenu");

  for(const syndicate of content) {
    let synP = document.createElement("p");
    let synA = document.createElement("a");
    synA.innerText = syndicate[0];
    synA.href = syndicate[1];
    synP.appendChild(synA);
    menu.appendChild(synP);
  }

  const popperInstance = Popper.createPopper(elem, menu, {
    modifiers: [
      { name: "preventOverflow",
        options: {}
      }
    ]
  });
  if(setupSynContentMenu.menu) {
    setupSynContentMenu.menu.remove();
  }
  setupSynContentMenu.menu = menu;
  const unregisterMenu = function () {
    setupSynContentMenu.menu = null;
    menu.remove();
  }
  menu.onmouseleave = () => unregisterMenu();
  document.body.appendChild(menu);
  setTimeout(() => document.body.addEventListener("click", (ev) => cleanSynContentMenu(ev, menu, unregisterMenu), false), 10);
}
