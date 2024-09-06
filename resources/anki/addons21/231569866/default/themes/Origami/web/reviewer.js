(() => {
  // { [key: "again" | "hard" | "good" | "easy"]: string[] }
  const images = {}

  let feedbackTimeout = null
  let waitingForStartImages = false

  function randomImageURL (array) {
    if (typeof array === 'undefined' || array.length === 0) return null
    return array[Math.floor(Math.random() * array.length)]
  }

  // Wait for pycmd to initialize
  function retrieveImages () {
    if (typeof pycmd === 'undefined') {
      setTimeout(retrieveImages, 10)
      return
    }
    for (const ease of ['again', 'hard', 'good', 'easy']) {
      window.pycmd('audiovisualFeedback#files#images/' + ease, (msg) => { images[ease] = JSON.parse(msg) })
    }
    window.pycmd('audiovisualFeedback#files#images/start', (msg) => {
      images.start = JSON.parse(msg)
      if (waitingForStartImages) window.avfReviewStart()
    })
  }

  function onLoad () {
    const div = document.createElement('div')
    div.id = 'visualFeedback'
    document.body.appendChild(div)
  }

  function showImage (array) {
    const container = document.getElementById('visualFeedback')
    if (feedbackTimeout) {
      clearTimeout(feedbackTimeout)
    }

    const imgUrl = randomImageURL(array)
    if (imgUrl === null) return

    const img = document.createElement('img')
    img.src = imgUrl
    container.appendChild(img)
    container.classList.add('visible')

    feedbackTimeout = setTimeout(() => {
      container.classList.remove('visible')
      container.removeChild(img)
    }, 200)
  }

  window.avfAnswer = (ease) => {
    showImage(images[ease])
  }

  window.avfReviewStart = () => {
    if ('start' in images) {
      showImage(images.start)
    } else {
      waitingForStartImages = true
    }
  }

  document.readyState === 'complete' ? onLoad() : window.addEventListener('load', onLoad)
  retrieveImages()
})()
