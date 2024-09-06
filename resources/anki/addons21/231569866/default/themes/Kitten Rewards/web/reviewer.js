(() => {
  let images
  let container
  let image

  let feedbackTimeout = null

  function randomImageURL () {
    if (images.length === 0) return null
    return images[Math.floor(Math.random() * images.length)]
  }

  // Wait for pycmd to initialize
  function retrieveImages () {
    if (typeof pycmd === 'undefined') {
      setTimeout(retrieveImages, 10)
      return
    }
    window.pycmd('audiovisualFeedback#files#images', (msg) => { images = JSON.parse(msg) })
  }

  function onLoad () {
    container = document.createElement('div')
    container.id = 'visualFeedback'
    document.body.appendChild(container)

    image = document.createElement('img')
    container.appendChild(image)
  }

  function showImage () {
    if (feedbackTimeout) {
      clearTimeout(feedbackTimeout)
    }

    const imgUrl = randomImageURL()
    if (imgUrl === null) return
    image.src = imgUrl
    container.classList.add('visible')

    feedbackTimeout = setTimeout((c) => {
      container.classList.remove('visible')
    }, 2000)
  }

  window.avfAnswer = (ease) => {
    if (ease !== 'good' && ease !== 'easy') { return }
    showImage()
  }

  document.readyState === 'complete' ? onLoad() : window.addEventListener('load', onLoad)
  retrieveImages()
})()
