def remove_comments(code):
    a, b = None, None
    cnt = 0
    new_code = []

    line, column = 1, 1
    for char in code:
        a, b = b, char

        if a == '(' and b == '*':
            if cnt == 0:
                new_code[-1] = ' '

            cnt += 1

        new_code.append(' ' if cnt and b != '\n' else b)

        if cnt > 0 and a == '*' and b == ')':
            cnt -= 1

        line += char == '\n'
        column = column * (char != '\n') + 1

    if cnt:
        return True, (line, column)

    return False, ''.join(new_code)