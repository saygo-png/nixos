(() => {
  function onLoad () {
    const div = document.createElement('div')
    div.id = 'avf-img-container'

    const img = document.createElement('img')
    img.id = 'avf-img'
    window.pycmd('audiovisualFeedback#randomFile#images', (src) => {
      if (src == null) return
      img.src = src
      div.appendChild(img)
      document.body.insertBefore(div, document.body.firstChild)
    })
  }

  document.readyState === 'complete' ? onLoad() : window.addEventListener('load', onLoad)
})()
