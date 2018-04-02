'use strict'

const xx = -1
const yy = 0
const zz = 1
const kk = -2
const ei = 8

class Reversi {
    constructor(players, other) {
        this.players = players
        this.gamedb = other[zz]
        this.watching = []
        this.cid = other[yy]
        this.directions =
           [[xx, xx], [xx, yy], [xx, zz],
             [yy, xx], [yy, zz],
             [zz, xx], [zz, yy], [zz, zz]]

        this.faces = [yy, zz]
        this.turn = yy
        this.Board =
            [yy, yy, yy, yy, yy, yy, yy, yy,
             yy, yy, yy, yy, yy, yy, yy, yy,
             yy, yy, yy, yy, yy, yy, yy, yy,
             yy, yy, yy, zz, xx, yy, yy, yy,
             yy, yy, yy, xx, zz, yy, yy, yy,
             yy, yy, yy, yy, yy, yy, yy, yy,
             yy, yy, yy, yy, yy, yy, yy, yy,
             yy, yy, yy, yy, yy, yy, yy, yy]
    }
    encodeCustom() {
        const hash = { cid:this.cid, turn:this.turn, board:this.Board }
        return hash
    }
    setBoard(board) {
        this.Board = board
    }
    setTurn(turn) {
        this.turn = turn
    }
    toPos(arr) {
        return arr[yy] * ei + arr[zz]
    }
    exRow(pos) {
        return Math.floor(pos / ei)
    }
    exCol(pos) {
        return pos % ei
    }
    emitBoard(ss) {
        ss.emit('board', this.Board)
    }

    updateBoard(array, player) {
        array.forEach((pos) => {
            this.Board[pos] = player
        })
    }

    adder(pos, dir) {
        return [pos[yy] + dir[yy], pos[zz] + dir[zz]]
    }
    helpCond(po) {
        const se = ei - zz
        const one = po[zz] >= yy && po[zz] <= se
        const two = po[yy] >= yy && po[yy] <= se
        return one && two
    }

    movesGen(row, col, player) {
        let result = []
        const oponent = - player
        if (this.Board[this.toPos([row, col])] === yy) {
            this.directions.forEach((dir) => {
                const subList = []
                let po = this.adder([row, col], dir)

                while (this.helpCond(po)) {
                    if (this.Board[this.toPos(po)] === oponent) {
                        subList.push(this.toPos(po))
                        po = this.adder(po, dir)
                    } else {
                        if (this.Board[this.toPos(po)] === player) {
                            result = result.concat(subList)
                        }
                        break
                    }
                }
            })
        }
        return result
    }

    helpMove(move) {
        let result = []
        const player = zz + kk * this.turn
        result = this.movesGen(this.exRow(move), this.exCol(move), player)
        if (result.length > yy) {
            result.push(move)
            this.updateBoard(result, player)
            return true
        }
        return false
    }

    allPossibleMoves(player) {
        const Result = []
        for (let Row = yy; Row < ei; Row++) {
            for (let Col = yy; Col < ei; Col++) {
                const Move = this.movesGen(Row, Col, player)
                if (Move.length > yy) {
                    Result.push(this.toPos([Row, Col]))
                }
            }
        }
        if (Result.length > yy) {
            return true
        }
        return false
    }
    
    gameOver() {
        const player = zz + kk * this.turn
        const aa = this.allPossibleMoves(player)
        const bb = this.allPossibleMoves(-player)
        if (!aa && !bb) {
            return true
        }
        return false
    }
    nextMove() {
        const player = zz + kk * this.turn
        const bb = this.allPossibleMoves(-player)
        if (bb) {
            this.turn = (this.turn + zz) % this.players.length
            this.players[this.turn].emit('move', 'Your move')
        }
    }
    checkNumb(player) {
        let count = yy
        this.Board.forEach((val) => {
            if (val === player) {
                count++
            }
        })
        return count
    }
    winCond() {
        const player = zz + kk * this.turn
        this.gamedb.remove({cid:this.cid}, () => {
            const opt = (this.turn + zz) % this.players.length
            if (this.checkNumb(player) > this.checkNumb(-player)) {
                this.players[this.turn].emit('over', 'Game over you won')
                this.players[opt].emit('over', 'Game over you lost')
            } else if (this.checkNumb(player) < this.checkNumb(-player)) {
                this.players[this.turn].emit('msg', 'Game over you lost')
                this.players[opt].emit('over', 'Game over you won')
            } else {
                this.players.forEach((socks) => {
                    socks.emit('over', 'Game Drawn')
                })
            }
        })
    }
    doMove(move, ii, ss) {
        if (ii === this.turn) {
            const isValid = this.helpMove(move)
            if (isValid) {
                this.nextMove()
                this.serializer()
                this.players.forEach((socks) => this.emitBoard(socks))
            } else {
                ss.emit('invalid', 'Invalid Move try again')
            }
            const over = this.gameOver()
            if (over) {
                this.winCond()
            }
        }
    }
    serializer() {
        const tofind = this.cid
        const upBoard = this.Board
        const upTurn = this.turn
        const update = { $set: { board: upBoard, turn:upTurn }}
        this.gamedb.update({cid:tofind}, update, (err, d) => {
            console.log(d)
        })
    }
    initSockets() {
        this.players[yy].emit('move', 'Your move')
        this.players.forEach((ss, ii) => {
            ss.emit('gid', this.cid)
            ss.emit('msg', 'Lets Begin!!')
            ss.emit('face', this.faces[ii])
            this.emitBoard(ss)
            ss.on('turn', (tt) => {
                this.doMove(tt, ii, ss)
            })
            ss.on('disconnect', () => {
                const opt = (ii + zz) % this.players.length
                this.gamedb.remove({cid:this.cid}, () => {
                    this.players[opt].emit('msg', 'Other Player Disconnected')
                })
            })
        })
    }
}

module.exports = Reversi
