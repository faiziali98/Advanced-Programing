'use strict'

const http = require('http')
const express = require('express')
const socketio = require('socket.io')
const Game = require('./Reversi.js')
const app = express()
const server = http.createServer(app)
const io = socketio(server)
const port = 8080
const mongoose = require('mongoose')
mongoose.connect('mongodb://localhost/games')

const gameSchema = new mongoose.Schema({
    cid : String,
    turn : Number,
    board : { type : Array, 'default' : [] },
}, {collection:'games'})

const Gamedb = mongoose.model('Game', gameSchema)

io.on('connection', onConnection)
app.use(express.static(__dirname))
server.listen(port, () => console.log('Ready to work!'))

const zro = 0

let waitingPlayer = null
const pending = {}

function onConnection(sock) {
    sock.on('myId', (data) => {
        const tofind = data.cid
        if (pending[tofind]) {
            pending[tofind][data.clf] = sock
            Gamedb.find({cid:tofind}, (err, d) => {
                const ngame = new Game(pending[tofind], [tofind, Gamedb])
                ngame.setTurn(d[zro].turn)
                ngame.setBoard(d[zro].board)
                ngame.initSockets()
                console.log('game restarted')
            })
        } else {
            const toadd = []
            toadd[data.clf] = sock
            pending[tofind] = toadd
            console.log('player1')
        }
    })
    sock.on('new', (data) => {
        if (data === null) {
            if (waitingPlayer) {
                const game = new Game(
                  [waitingPlayer, sock], [sock.id, Gamedb])
                const gm = new Gamedb(game.encodeCustom())
                gm.save()
                game.initSockets()
                waitingPlayer = null
            } else {
                waitingPlayer = sock
                sock.emit('msg', 'Waiting for other player')
            }
        }
    })
}
