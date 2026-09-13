function cleanHdMenu(ev, menu, cleaner) {
  document.body.removeEventListener("click", (ev) => cleanHdMenu(ev, menu, cleaner));
  cleaner();
}

function sendHeaderAction(actUrl, redirUrl, deepEmbed, extra, action) {
  fetch(actUrl, {
    method: "POST",
    body: redirUrl + "\n" + deepEmbed + "\n" + action + (extra ? "\n" + extra : "")
  }).then((response) => response.text()).then((url) => {
    if(url) {
      window.location = url;
    }
  });
}

function setupHeaderMenu(buttonId, editFn, editFirstFn, openUrl, actUrl, redirUrl, deepEmbed) {
  const elem = document.getElementById(buttonId);
  if(!editFn && !editFirstFn && !openUrl && !actUrl) {
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
      edit.addEventListener("click", (ev) => {
        elem.parentElement.parentElement.classList.remove("collapsed");
        editFn()
      });
      menu.appendChild(edit);
    }

    if(editFirstFn) {
      let edit = document.createElement("p");
      edit.innerText = "Edit first";
      edit.addEventListener("click", (ev) => {
        elem.parentElement.parentElement.classList.remove("collapsed");
        editFirstFn()
      });
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
        elem.addEventListener("click", () => sendHeaderAction(actUrl, redirUrl, deepEmbed, null, act));
        menu.appendChild(elem);
      };
      createSub("New first sub", "sub-first");
      createSub("New last sub", "sub-last");
      createSub("Header after", "header-after");
      createSub("Header before", "header-before");
      createSub("Finish task", "finish-task");

      const createTimePrompt = (mtdt) => {
        let timePrompt = document.createElement("p");
        timePrompt.innerText = "Set " + mtdt;
        timePrompt.addEventListener("click", () => subTimePrompt(elem, actUrl, deepEmbed,mtdt));
        menu.appendChild(timePrompt);
      };
      createTimePrompt("scheduled");
      createTimePrompt("deadline");
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

function subTimePrompt(elem, actUrl, deepEmbed, mtdt) {
  let calendar = document.createElement("div");
  calendar.id = "hdcalendarinput";
  calendar.classList.add("contextmenu");

  let input = document.createElement("input");
  input.type = "datetime-local";
  calendar.appendChild(input);

  let button = document.createElement("button");
  button.innerText = "Set";
  calendar.appendChild(button);

  const popperInstance = Popper.createPopper(elem, calendar, {
    modifiers: [
      { name: "preventOverflow",
        options: {}
      }
    ]
  });
  if(subTimePrompt.menu) {
    subTimePrompt.menu.remove();
  }
  subTimePrompt.menu = calendar;
  const unregisterMenu = function () {
    subTimePrompt.menu = null;
    calendar.remove();
  }
  document.body.appendChild(calendar);

  button.addEventListener("click", () => {
    let timestamp = Date.parse(input.value);
    unregisterMenu();
    if(isNaN(timestamp) == false) {
      let date = new Date(timestamp).toISOString();
      sendHeaderAction(actUrl, "", deepEmbed, mtdt + "\n" + date, "set-metadata");
    }
  });
}
