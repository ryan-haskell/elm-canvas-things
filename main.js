import { Elm } from './src/Main.elm'

// Start the Elm app
const app = Elm.Main.init({
  node: document.getElementById('app')
})

// Image fetching helper
const images = {
  cache: {},
  fetch (url) {
    return new Promise(resolve => {
      if (this.cache[url]) {
        resolve(this.cache[url])
      } else {
        const image = new window.Image()
        image.src = url
        image.onload = _ => {
          resolve(image)
          this.cache[url] = image
        }
      }
    })
  },
  prefetch (urls) {
    return Promise.all(urls.map(url => this.fetch(url)))
      .then(images =>
        images.reduce((obj, image, i) =>
          (obj[urls[i]] = image) && obj
        , {})
      )
  }
}

// Canvas API helper
const renderWith = (ctx) => (images) => ({
  image: ({ url, x, y, width, height, sprite = {}}) => {
    ctx.imageSmoothingEnabled = false
    ctx.drawImage(...[
      images[url],
      sprite.x, sprite.y,
      sprite.width, sprite.height,
      x, y,
      width, height
    ].filter(a => a !== undefined))
  },
  rectangle: ({ x, y, width, height, color }) => {
    ctx.fillStyle = color
    ctx.fillRect(x, y, width, height)
  },
  polygon: ({ path, color }) => {
    const [ head, ...tail ] = path
    ctx.fillStyle = color
    ctx.beginPath()
    ctx.moveTo(...head)
    tail.forEach(pair => ctx.lineTo(...pair))
    ctx.closePath()
    ctx.fill()
  }
})

// Function that renders from data
const canvas = document.querySelector('#canvas')

const urlsFor = (items) =>
  items
    .filter(({ tag }) => tag === 'image')
    .map(({ args })=> args[0].url)
    .filter(a => a !== undefined)

const draw = ({ size: { width, height }, background, items }) => {
  canvas.width = width
  canvas.height = height
  images.prefetch(urlsFor(items)).then(images => {
    const renderer = renderWith(canvas.getContext('2d'))(images)
    renderer.rectangle({ x: 0, y: 0, width, height, color: background })
    items.forEach(({ tag, args }) => renderer[tag](args[0]))
  })
}

// Touch controls because keyboards are not on phones :hmm:
function touchSupport (callback) {
  const el = canvas
  const position = ({ touches }) => ({ x: touches[0].screenX, y: touches[0].screenY })
  const init = (event) => event ? position(event) : { x: 0, y: 0 }
  const endTouch = _ => {
    callback({ x: 0, y: 0 })
    return false
  }
  
  let model = init()
  el.addEventListener("touchstart", (event) => {
    model = init(event)
    return false
  }, false)
  el.addEventListener("touchend", endTouch, false)
  el.addEventListener("touchcancel", endTouch, false)
  el.addEventListener("touchmove", (event) => {
    const point = position(event)
    const x = Math.abs(point.x - model.x)
    const y = Math.abs(point.y - model.y)
    const max = Math.max(x, y)
    const normalized = {
      x: x / max * (point.x > model.x ? 1 : -1),
      y: y / max * (point.y > model.y ? 1 : -1)
    }
    callback(normalized)
    return false
  }, false)
}
touchSupport(app.ports.incoming.send)


// An example of what Elm would send
app.ports.outgoing.subscribe(({ action, payload }) => {
  switch (action) {
    case 'RENDER': return draw(payload)
  }
})
