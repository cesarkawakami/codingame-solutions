import itertools
import math
import sys
import copy


width, height = [int(i) for i in input().split()]
board = []  # board[row][column]
bomb_positions = set()
for i in range(height):
    map_row = list(input().strip())  # one line of the firewall grid
    board.append(map_row)
    print(map_row, file=sys.stderr)


dx = [0, 0, -1, +1]
dy = [-1, +1, 0, 0]


def will_be_exploded_anyway(r, c):
    for direction in range(4):
        for k in range(1, 4):
            nr, nc = r + dx[direction] * k, c + dy[direction] * k
            if not (0 <= nr < height) or not (0 <= nc < width):
                break
            if board[nr][nc] == "#":
                break
            if isinstance(board[nr][nc], int):
                return True
    return False


def update_bomb_positions(r, c):
    if isinstance(board[r][c], int):
        bomb_positions.add((r, c))
    else:
        bomb_positions.discard((r, c))


def explode(r, c, modify=False):
    # if we put a bomb on (r, c), how many nodes do we explode?
    explosion_count = 0
    if modify:
        board[r][c] = "."
        update_bomb_positions(r, c)

    for direction in range(4):
        for k in range(1, 4):
            nr, nc = r + dx[direction] * k, c + dy[direction] * k
            if not (0 <= nr < height) or not (0 <= nc < width):
                break
            if board[nr][nc] == "#":
                break
            if board[nr][nc] == "@":
                if modify:
                    board[nr][nc] = "."
                    update_bomb_positions(nr, nc)
                if not will_be_exploded_anyway(nr, nc):
                    explosion_count += 1
            if isinstance(board[nr][nc], int) and modify:
                explode(nr, nc, modify)
    return explosion_count


def place(round_count, bomb_count, move):
    old_board = copy.deepcopy(board)
    old_bomb_positions = set(bomb_positions)

    if move is not None:
        r, c = move
        board[r][c] = 3
        update_bomb_positions(r, c)

    for r, c in set(bomb_positions):
        board[r][c] -= 1
        update_bomb_positions(r, c)
        if board[r][c] == 0:
            explode(r, c, True)

    rv = go(round_count - 1, bomb_count - 1 if move is not None else bomb_count)

    bomb_positions.clear()
    bomb_positions.update(old_bomb_positions)
    board[:] = old_board[:]
    return rv


def go(round_count, bomb_count):
    if not round_count:
        return None

    node_count = sum(1 for r in range(height) for c in range(width) if board[r][c] == "@")
    if not node_count:
        print("found:", board, file=sys.stderr)
        return -1, -1

    candidates = []
    for r, c in itertools.product(range(height), range(width)):
        if board[r][c] != ".":
            continue
        evaluation = explode(r, c)
        candidates.append((evaluation, r, c))

    candidates.sort(reverse=True)
    # print("rc:", round_count, "bc:", bomb_count, "c:", candidates[:2], file=sys.stderr)

    if bomb_count:
        for _, r, c in candidates[:1]:
            if place(round_count, bomb_count, (r, c)):
                return r, c

    if place(round_count, bomb_count, None):
        return -1, -1

    return None


while True:
    rounds, bombs = [int(i) for i in input().split()]
    print("rounds:", rounds, "bombs:", bombs, file=sys.stderr)

    print("board:", file=sys.stderr)
    for rr in range(height):
        print("".join(str(x) for x in board[rr]), file=sys.stderr)

    r, c = go(rounds, bombs)

    if (r, c) == (-1, -1):
        print("WAIT")
    else:
        board[r][c] = 3
        update_bomb_positions(r, c)
        print(c, r)

    for r, c in itertools.product(range(height), range(width)):
        if isinstance(board[r][c], int):
            board[r][c] -= 1
            update_bomb_positions(r, c)
            if board[r][c] == 0:
                explode(r, c, True)

