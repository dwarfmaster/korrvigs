function changeCheckbox(cid, postUrl, prev, nw, newUrl, menu) {
  menu.remove()
  fetch(postUrl, {
    method: "POST",
    body: nw,
    headers: {
      "Content-Type": "text/plain; charset=UTF-8"
    }
  })

  let toChange = [0,0,0,0,0]
  if(prev == "todo") { toChange[0] -= 1 }
  else if(prev == "ongoing") { toChange[1] -= 1 }
  else if(prev == "blocked") { toChange[2] -= 1 }
  else if(prev == "done") { toChange[3] -= 1 }
  else { toChange[4] -= 1 }
  if(nw == "todo") { toChange[0] += 1 }
  else if(nw == "ongoing") { toChange[1] += 1 }
  else if(nw == "blocked") { toChange[2] += 1 }
  else if(nw == "done") { toChange[3] += 1 }
  else { toChange[4] += 1}
 
  const spans = document.querySelectorAll(`section:has(#${cid}):not(:has(.task-section #${cid})) > :is(h2,h3,h4,h5,h6) > .checks-count`)
  spans.forEach((span) => checkboxUpdateSpan(span, toChange))
  let topspan = document.querySelector(`div:not(:has(.task-section #${cid})) > .checks-top > .checks-count`)
  if(topspan) {
    checkboxUpdateSpan(topspan, toChange)
  }
  document.getElementById(cid).setAttribute("src", newUrl)
}

function checkboxUpdateSpan(span, toChange) {
  for(var child of span.children) {
    if(child.classList.contains("todo-count")) {
      child.innerText = (parseInt(child.innerText) + toChange[0]).toString()
    } else if(child.classList.contains("ongoing-count")) {
      child.innerText = (parseInt(child.innerText) + toChange[1]).toString()
    } else if(child.classList.contains("blocked-count")) {
      child.innerText = (parseInt(child.innerText) + toChange[2]).toString()
    } else if(child.classList.contains("done-count")) {
      child.innerText = (parseInt(child.innerText) + toChange[3]).toString()
    } else if(child.classList.contains("dont-count")) {
      child.innerText = (parseInt(child.innerText) + toChange[4]).toString()
    }
  }
}

function checkboxStatus(url, todoUrl, ongoingUrl, blockedUrl, doneUrl, dontUrl) {
  if(url == todoUrl) {
    return "todo"
  } else if(url == ongoingUrl) {
    return "ongoing"
  } else if(url == blockedUrl) {
    return "blocked"
  } else if(url == doneUrl) {
    return "done"
  } else {
    return "dont"
  }
}

function setupCheckbox(postUrl, todoUrl, ongoingUrl, blockedUrl, doneUrl, dontUrl, id) {
  const checkbox = document.getElementById(id)
  checkbox.addEventListener('contextmenu', function(ev) {
    const url = checkbox.getAttribute("src")
    const st = checkboxStatus(url, todoUrl, ongoingUrl, blockedUrl, doneUrl, dontUrl)
    ev.preventDefault()

    let menu = document.createElement("div")
    menu.id = "checkboxcontextmenu"
    menu.style = `top:${ev.clientY-10}px;left:${ev.clientX-40}px`
    menu.onmouseleave = () => menu.remove()

    if(st != "todo") {
      let p = document.createElement("p")
      p.innerText = "Todo"
      p.onclick = () => changeCheckbox(id, postUrl, st, "todo", todoUrl, menu)
      menu.appendChild(p)
    }
    if(st != "ongoing") {
      let p = document.createElement("p")
      p.innerText = "Ongoing"
      p.onclick = () => changeCheckbox(id, postUrl, st, "ongoing", ongoingUrl, menu)
      menu.appendChild(p)
    }
    if(st != "blocked") {
      let p = document.createElement("p")
      p.innerText = "Blocked"
      p.onclick = () => changeCheckbox(id, postUrl, st, "blocked", blockedUrl, menu)
      menu.appendChild(p)
    }
    if(st != "done") {
      let p = document.createElement("p")
      p.innerText = "Done"
      p.onclick = () => changeCheckbox(id, postUrl, st, "done", doneUrl, menu)
      menu.appendChild(p)
    }
    if(st != "dont") {
      let p = document.createElement("p")
      p.innerText = "Cancel"
      p.onclick = () => changeCheckbox(id, postUrl, st, "dont", dontUrl, menu)
      menu.appendChild(p)
    }

    document.body.appendChild(menu)
    document.body.addEventListener("click", (ev) => cleanCheckboxMenu(ev, menu))
    return false
  })
}

function cleanCheckboxMenu(ev, menu) {
  if(!menu.contains(ev.target)) {
    document.body.removeEventListener("click", (ev) => cleanCheckboxMenu(ev, menu))
    menu.remove()
  }
}
