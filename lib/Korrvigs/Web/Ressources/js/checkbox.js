function propagateChecks(cid, toChange) {
  const spans = document.querySelectorAll(`section:has(#${cid}):not(:has(.task-section #${cid})) > :is(h2,h3,h4,h5,h6) > .checks-count`)
  spans.forEach((span) => {
    if(!span.parentElement.querySelector(`:scope > #${cid}`)) {
      checkboxUpdateSpan(span, toChange)
    }
  })

  let topspan = document.querySelector(`div:not(:has(.task-section #${cid})) > .checks-top > .checks-count`)
  if(topspan) {
    checkboxUpdateSpan(topspan, toChange)
  }
}

function updateChecks(cid, prev, nw) {
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
  propagateChecks(cid, toChange)
}

function changeCheckbox(cid, postUrl, prev, nw, newUrl) {
  fetch(postUrl, {
    method: "POST",
    body: nw,
    headers: {
      "Content-Type": "text/plain; charset=UTF-8"
    }
  })
  updateChecks(cid, prev, nw)
  document.getElementById(cid).setAttribute("src", newUrl)
}

function changeTask(id, prev, nw, stat) {
  let span = document.getElementById(id)
  let secId = span.parentElement.parentElement.id
  updateChecks(secId, prev, nw)
  span.classList.remove(`task-${prev}`)
  span.classList.add(`task-${nw}`)
  span.textContent = stat
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

function checkboxCleanSpans() {
  for(let span of document.getElementsByClassName("checks-count")) {
    var isEmpty = true
    for(var child of span.children) {
      if(child.classList.contains("todo-count")
        || child.classList.contains("ongoing-count")
        || child.classList.contains("blocked-count")
        || child.classList.contains("done-count")
        || child.classList.contains("dont-count")) {
        const val = parseInt(child.innerText)
        if(val > 0) {
          isEmpty = false
          break
        }
      }
    }
    if(isEmpty) {
      span.remove()
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

function createCheckboxMenu(st, clickFn) {
    let menu = document.createElement("div")
    menu.id = "checkboxcontextmenu"
    menu.onmouseleave = () => menu.remove()

    if(st != "todo") {
      let p = document.createElement("p")
      p.innerText = "Todo"
      p.onclick = () => clickFn("todo")
      menu.appendChild(p)
    }
    if(st != "ongoing") {
      let p = document.createElement("p")
      p.innerText = "Ongoing"
      p.onclick = () => clickFn("ongoing")
      menu.appendChild(p)
    }
    if(st != "blocked") {
      let p = document.createElement("p")
      p.innerText = "Blocked"
      p.onclick = () => clickFn("blocked")
      menu.appendChild(p)
    }
    if(st != "done") {
      let p = document.createElement("p")
      p.innerText = "Done"
      p.onclick = () => clickFn("done")
      menu.appendChild(p)
    }
    if(st != "dont") {
      let p = document.createElement("p")
      p.innerText = "Cancel"
      p.onclick = () => clickFn("dont")
      menu.appendChild(p)
    }

    document.body.appendChild(menu)
    document.body.addEventListener("click", (ev) => cleanCheckboxMenu(ev, menu), false)
    return menu
}

function setupCheckbox(postUrl, todoUrl, ongoingUrl, blockedUrl, doneUrl, dontUrl, id) {
  const checkbox = document.getElementById(id)
  checkbox.addEventListener('contextmenu', function(ev) {
    const url = checkbox.getAttribute("src")
    const st = checkboxStatus(url, todoUrl, ongoingUrl, blockedUrl, doneUrl, dontUrl)
    ev.preventDefault()

    let clickFn = function(nst) {
      if(nst == "todo") {
        changeCheckbox(id, postUrl, st, "todo", todoUrl)
      } else if(nst == "ongoing") {
        changeCheckbox(id, postUrl, st, "ongoing", ongoingUrl)
      } else if(nst == "blocked") {
        changeCheckbox(id, postUrl, st, "blocked", blockedUrl)
      } else if(nst == "done") {
        changeCheckbox(id, postUrl, st, "done", doneUrl)
      } else {
        changeCheckbox(id, postUrl, st, "dont", dontUrl)
      }
    }
    let menu = createCheckboxMenu(st, clickFn)
    menu.style = `top:${ev.clientY-10}px;left:${ev.clientX-40}px`
    return false
  })
}

function cleanCheckboxMenu(ev, menu) {
  document.body.removeEventListener("click", (ev) => cleanCheckboxMenu(ev, menu))
  menu.remove()
}

function setupTask(postUrl, id) {
  const span = document.getElementById(id)
  span.addEventListener('contextmenu', function(ev) {
    ev.preventDefault()
    let st = "dont"
    if(span.classList.contains('task-todo')) {
      st = "todo"
    } else if(span.classList.contains('task-ongoing')) {
      st = "ongoing"
    } else if(span.classList.contains('task-blocked')) {
      st = "blocked"
    } else if(span.classList.contains('task-done')) {
      st = "done"
    }

    let clickFn = function(nst) {
      let nwName = nst
      if(nwName == "ongoing") {
        nwName = "started"
      }
      fetch(postUrl, {
        method: "POST",
        body: nwName,
        headers: {
          "Content-Type": "text/plain; charset=UTF-8"
        }
      })
      changeTask(id, st, nst, nwName)
    }

    let menu = createCheckboxMenu(st, clickFn)
    menu.style = `top:${ev.clientY-10}px;left:${ev.clientX-40}px`
    return false
  })
}

function setupTopTask(postUrl, id, entryId) {
  const span = document.getElementById(id)
  span.addEventListener('contextmenu', function(ev) {
    ev.preventDefault()
    let st = "dont"
    if(span.classList.contains('task-todo')) {
      st = "todo"
    } else if(span.classList.contains('task-ongoing')) {
      st = "ongoing"
    } else if(span.classList.contains('task-blocked')) {
      st = "blocked"
    } else if(span.classList.contains('task-done')) {
      st = "done"
    }

    let clickFn = function(nst) {
      let nwName = nst
      if(nwName == "ongoing") {
        nwName = "started"
      }
      fetch(postUrl, {
        method: "POST",
        body: JSON.stringify({ insert: { ["task"]: nwName }, remove: ["task"] }),
        headers: {
          "Content-Type": "application/json; charset=UTF-8"
        }
      })
      let mtdtVal = document.querySelector(`[data-mtdt-value-for="task"][data-mtdt-for="${entryId}"]`)
      if(mtdtVal) {
        mtdtVal.textContent = `"${nwName}"`
      }
      changeTask(id, st, nst, nwName)
    }

    let menu = createCheckboxMenu(st, clickFn)
    menu.style = `top:${ev.clientY-10}px;left:${ev.clientX-40}px`
    return false
  })
}
