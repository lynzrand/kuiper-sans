import opentype from 'opentype.js'
import arg from 'arg'
import fs from 'fs'

const args = arg({})
const buf = fs.readFileSync(args._[0])
const font_buf = new Uint8Array(buf).buffer
const font = opentype.parse(font_buf)
