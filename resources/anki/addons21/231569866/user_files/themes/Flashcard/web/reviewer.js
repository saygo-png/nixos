(() => {
  let feedbackTimeout = null

  function visualFeedbackDiv () {
    const div = document.createElement('div')
    div.classList.add('visualFeedback')
    document.body.appendChild(div)
    return div
  }

  function onLoad () {
    for (const pos of ['top', 'bottom', 'left', 'right']) {
      const div = visualFeedbackDiv()
      div.classList.add(pos)
    }
  }

  window.avfAnswer = (ease) => {
    const elems = document.getElementsByClassName('visualFeedback')
    if (feedbackTimeout) {
      clearTimeout(feedbackTimeout)
    }
    for (const elem of elems) {
      elem.classList.add(ease)
    }

    feedbackTimeout = setTimeout((c) => {
      for (const elem of elems) {
        elem.classList.remove(c)
      }
    }, 300, ease)
  }

  document.readyState === 'complete' ? onLoad() : window.addEventListener('load', onLoad)
})()
