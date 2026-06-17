function readItem(element, readUrl, liId, spanId) {
  fetch(readUrl).then(function() {
    if(liId) {
      document.getElementById(liId).remove()
    } else {
      if(spanId) {
        document.getElementById(spanId).textContent = "✓";
      }
      element.remove()
    }
  })
}
