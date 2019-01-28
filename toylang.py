import argparse
from enum import Enum, auto


class TokenType(Enum):
    IDENTIFIER = auto()
    NUMBER = auto()
    STRING = auto()
    PLUS = auto()
    MINUS = auto()
    STAR = auto()
    SLASH = auto()
    PERCENT = auto()
    LEFT_BRACE = auto()
    RIGHT_BRACE = auto()
    LEFT_PAREN = auto()
    RIGHT_PAREN = auto()
    NOT = auto()
    EQUAL = auto()
    EQUAL_EQUAL = auto()
    NOT_EQUAL = auto()
    GREATER = auto()
    GREATER_EQUAL = auto()
    LOWER = auto()
    LOWER_EQUAL = auto()

    # Keywords
    IF = auto()
    ELSE = auto()
    WHILE = auto()
    AND = auto()
    OR = auto()
    TRUE = auto()
    FALSE = auto()
    PRINT = auto()

    EOF = auto()


class ToyLangException(Exception):
    pass


class Token:
    def __init__(self, token_type, lexeme, line, value):
        self.type = token_type
        self.lexeme = lexeme
        self.line = line
        self.value = value


class BinaryExpression:
    def __init__(self, left, operator, right):
        self.left = left
        self.operator = operator
        self.right = right

    def accept(self, visitor):
        return visitor.visit_binary_expression(self)


class UnaryExpression:
    def __init__(self, operator, operand):
        self.operator = operator
        self.operand = operand

    def accept(self, visitor):
        return visitor.visit_unary_expression(self)


class VariableExpression:
    def __init__(self, name):
        self.name = name

    def accept(self, visitor):
        return visitor.visit_variable_expression(self)


class LiteralExpression:
    def __init__(self, value):
        self.value = value

    def accept(self, visitor):
        return visitor.visit_literal_expression(self)


class StatementsStatement:
    def __init__(self, statements):
        self.statements = statements

    def accept(self, visitor):
        visitor.visit_statements_statement(self)


class IfStatement:
    def __init__(self, condition, if_true, if_false):
        self.condition = condition
        self.if_true = if_true
        self.if_false = if_false

    def accept(self, visitor):
        visitor.visit_if_statement(self)


class WhileStatement:
    def __init__(self, condition, body):
        self.condition = condition
        self.body = body

    def accept(self, visitor):
        visitor.visit_while_statement(self)


class AssignStatement:
    def __init__(self, name, value):
        self.name = name
        self.value = value

    def accept(self, visitor):
        visitor.visit_assign_statement(self)


class PrintStatement:
    def __init__(self, value):
        self.value = value

    def accept(self, visitor):
        visitor.visit_print_statement(self)


class Scanner:
    KEYWORDS = {
        'if': TokenType.IF, 'else': TokenType.ELSE, 'while': TokenType.WHILE, 'and': TokenType.AND, 'or': TokenType.OR,
        'true': TokenType.TRUE, 'false': TokenType.FALSE, 'print': TokenType.PRINT
    }

    def __init__(self, source):
        self.source = source
        self.tokens = list()
        self.string_start = 0
        self.current = 0
        self.line = 1
        self.current_string = None

    @property
    def is_eof(self):
        return self.current == len(self.source)

    @property
    def current_char(self):
        if self.is_eof:
            return '\0'
        return self.source[self.current]

    @property
    def current_lexeme(self):
        return self.source[self.string_start:self.current]

    @property
    def next_char(self):
        self.current += 1
        return_value = self.current_char
        self.current -= 1
        return return_value

    def scan_tokens(self):
        while not self.is_eof:
            self.string_start = self.current
            self.scan_token()

        self.add_token(TokenType.EOF)
        return self.tokens

    def scan_token(self):
        char = self.advance()

        if char == '(':
            self.add_token(TokenType.LEFT_PAREN)
        elif char == ')':
            self.add_token(TokenType.RIGHT_PAREN)
        elif char == '{':
            self.add_token(TokenType.LEFT_BRACE)
        elif char == '}':
            self.add_token(TokenType.RIGHT_BRACE)
        elif char == '*':
            self.add_token(TokenType.STAR)
        elif char == '%':
            self.add_token(TokenType.PERCENT)
        elif char == '-':
            self.add_token(TokenType.MINUS)
        elif char == '+':
            self.add_token(TokenType.PLUS)
        elif char == '=':
            if self.match('='):
                self.add_token(TokenType.EQUAL_EQUAL)
            else:
                self.add_token(TokenType.EQUAL)
        elif char == '!':
            self.add_token(TokenType.NOT_EQUAL if self.match('=') else TokenType.NOT)
        elif char == '>':
            self.add_token(TokenType.GREATER_EQUAL if self.match('=') else TokenType.GREATER)
        elif char == '<':
            self.add_token(TokenType.LOWER_EQUAL if self.match('=') else TokenType.LOWER)
        elif char == '/':
            if self.match('/'):
                while self.current_char != '\n' and not self.is_eof:
                    self.advance()
            else:
                self.add_token(TokenType.SLASH)
        elif char == '\n':
            self.line += 1
        elif char == ' ' or char == '\r' or char == '\t':
            return
        elif char == '"' or char == '\'':
            self.string(char)
        elif char.isdigit():
            self.number()
        elif char.isalpha():
            self.identifier()
        else:
            self.report('Unexpected character ' + char)

    def identifier(self):
        while self.current_char.isalnum():
            self.advance()

        identifier = self.current_lexeme
        tok_type = TokenType.IDENTIFIER

        if identifier in __class__.KEYWORDS:
            tok_type = __class__.KEYWORDS[identifier]

        self.add_token(tok_type, identifier)

    def number(self):
        while self.current_char.isdigit():
            self.advance()

        is_float = False

        if self.current_char == '.' and self.next_char.isdigit():
            is_float = True
            self.advance()
            while self.current_char.isdigit():
                self.advance()

        self.add_token(TokenType.NUMBER, float(self.current_lexeme) if is_float else int(self.current_lexeme))

    def string(self, delimiter):
        self.current_string = ''

        while self.current_char != delimiter and not self.is_eof:
            if self.current_char == '\n':
                self.line += 1
            elif self.current_char == '\\':
                self.advance()
                self.escape()
                continue

            self.current_string += self.advance()

        if self.is_eof:
            self.report('Unterminated string')
        else:
            self.advance()
            self.add_token(TokenType.STRING, self.current_string)

    def escape(self):
        if self.is_eof:
            self.report('Unexpected EOF')
            return

        if self.match('n'):
            self.current_string += '\n'
        elif self.match('t'):
            self.current_string += '\t'
        elif self.match('"'):
            self.current_string += '"'
        elif self.match("'"):
            self.current_string += "'"
        elif self.match('\\'):
            self.current_string += '\\'
        else:
            self.report('Bad escape character: ' + self.current_char)

    def match(self, to_match):
        if self.current_char == to_match:
            self.advance()
            return True
        return False

    def advance(self):
        self.current += 1
        return self.source[self.current - 1]

    def add_token(self, token_type, value=None):
        self.tokens.append(Token(token_type=token_type, value=value, lexeme=self.current_lexeme, line=self.line))

    def report(self, message):
        raise ToyLangException('[line %d] %s' % (self.line, message))


class Parser:

    def __init__(self, tokens):
        self.tokens = tokens
        self.statements = list()
        self.current = 0

    @property
    def previous_token(self):
        return self.tokens[self.current - 1]

    @property
    def current_token(self):
        return self.tokens[self.current]

    @property
    def is_eof(self):
        return self.current >= len(self.tokens) or self.current_token.type == TokenType.EOF

    def parse(self):
        while not self.is_eof:
            self.statements.append(self.parse_statement())

        return self.statements

    def parse_statement(self):
        if self.matches(TokenType.IDENTIFIER):
            return self.parse_assign_statement()
        elif self.matches(TokenType.PRINT):
            return self.parse_print_statement()
        elif self.matches(TokenType.IF):
            return self.parse_if_statement()
        elif self.matches(TokenType.WHILE):
            return self.parse_while_statement()
        elif self.matches(TokenType.LEFT_BRACE):
            return self.parse_statements_statement()

    def parse_statements_statement(self):
        self.consume(TokenType.LEFT_BRACE, "Was expecting '{'")
        statements = list()

        while not self.match(TokenType.RIGHT_BRACE):
            statements.append(self.parse_statement())

        return StatementsStatement(statements)

    def parse_assign_statement(self):
        name = self.consume(TokenType.IDENTIFIER, "Was expecting an identifier").lexeme
        self.consume(TokenType.EQUAL, "Was expecting '='")
        return AssignStatement(name, self.parse_expression())

    def parse_print_statement(self):
        self.consume(TokenType.PRINT, "Was expecting 'print")
        return PrintStatement(self.parse_expression())

    def parse_if_statement(self):
        self.consume(TokenType.IF, "Was expecting 'if'")
        condition = self.parse_expression()
        if_true = self.parse_statements_statement()
        if_false = None
        if self.match(TokenType.ELSE):
            if self.lookahead(TokenType.IF):
                if_false = self.parse_statement()
            else:
                if_false = self.parse_statements_statement()
        return IfStatement(condition, if_true, if_false)

    def parse_while_statement(self):
        self.consume(TokenType.WHILE, "Was expecting 'while'")
        return WhileStatement(self.parse_expression(), self.parse_statements_statement())

    def parse_expression(self):
        return self.parse_or_condition()

    def parse_or_condition(self):
        expression = self.parse_and_condition()

        while self.match(TokenType.OR):
            expression = BinaryExpression(expression, self.previous_token, self.parse_and_condition())

        return expression

    def parse_and_condition(self):
        expression = self.parse_condition()

        while self.match(TokenType.AND):
            expression = BinaryExpression(expression, self.previous_token, self.parse_condition())

        return expression

    def parse_condition(self):
        expression = self.parse_addition()

        if self.match_any((TokenType.LOWER, TokenType.LOWER_EQUAL, TokenType.GREATER, TokenType.GREATER_EQUAL,
                           TokenType.EQUAL_EQUAL, TokenType.NOT_EQUAL)):
            expression = BinaryExpression(expression, self.previous_token, self.parse_addition())

        return expression

    def parse_addition(self):
        expression = self.parse_multiplication()

        while self.match(TokenType.PLUS) or self.match(TokenType.MINUS):
            expression = BinaryExpression(expression, self.previous_token, self.parse_multiplication())

        return expression

    def parse_multiplication(self):
        expression = self.parse_unary()

        while self.match_any((TokenType.STAR, TokenType.SLASH, TokenType.PERCENT)):
            expression = BinaryExpression(expression, self.previous_token, self.parse_unary())

        return expression

    def parse_unary(self):
        if self.match(TokenType.NOT) or self.match(TokenType.MINUS):
            return UnaryExpression(self.previous_token, self.parse_expression())

        return self.parse_factor()

    def parse_factor(self):
        if self.match(TokenType.IDENTIFIER):
            return VariableExpression(self.previous_token.lexeme)
        elif self.match(TokenType.NUMBER):
            return LiteralExpression(self.previous_token.value)
        elif self.match(TokenType.STRING):
            return LiteralExpression(self.previous_token.value)
        elif self.match(TokenType.TRUE):
            return LiteralExpression(True)
        elif self.match(TokenType.FALSE):
            return LiteralExpression(False)
        elif self.match(TokenType.LEFT_PAREN):
            expression = self.parse_expression()
            self.consume(TokenType.RIGHT_PAREN, "Was expecting ')")
            return expression

        raise ToyLangException(
            '[line %d] Unexpected token %s' % (self.current_token.line, self.current_token.lexeme))

    def matches(self, token_type):
        return not self.is_eof and self.current_token.type == token_type

    def match(self, token_type):
        if self.matches(token_type):
            self.advance()
            return True

        return False

    def match_any(self, token_types):
        for token_type in token_types:
            if self.matches(token_type):
                self.advance()
                return True

        return False

    def lookahead(self, token_type, by=1):
        self.current += by
        match = self.matches(token_type)
        self.current -= by

        return match

    def consume(self, token_type, failure_message):
        if not self.matches(token_type):
            raise ToyLangException(
                "[line %d] %s. Found '%s'" % (self.current_token.line, failure_message, self.current_token.lexeme))
        return self.advance()

    def advance(self):
        if not self.is_eof:
            self.current += 1
        return self.previous_token


class Interpreter:
    def __init__(self, statements):
        self.statements = statements
        self.symbol_table = dict()

    def run(self):
        for statement in self.statements:
            statement.accept(self)

    @staticmethod
    def is_number(value):
        return not isinstance(value, bool) and isinstance(value, (float, int))

    def visit_assign_statement(self, statement):
        self.symbol_table[statement.name] = self.evaluate(statement.value)

    def visit_if_statement(self, statement):
        if self.evaluate(statement.condition):
            statement.if_true.accept(self)
        elif statement.if_false:
            statement.if_false.accept(self)

    def visit_while_statement(self, statement):
        while self.evaluate(statement.condition):
            statement.body.accept(self)

    def visit_print_statement(self, statement):
        value = self.evaluate(statement.value)
        print(value)

    def visit_statements_statement(self, statements):
        for statement in statements.statements:
            statement.accept(self)

    def evaluate(self, expression):
        return expression.accept(self)

    def visit_literal_expression(self, expression):
        return expression.value

    def visit_unary_expression(self, expression):
        value = self.evaluate(expression.operand)
        if expression.operator.type == TokenType.NOT:
            if isinstance(value, bool):
                return not value
            else:
                raise ToyLangException(
                    "[line %d] '!' operator can only be applied to boolean values" % expression.operator.line)
        elif expression.operator.type == TokenType.MINUS:
            if self.is_number(value):
                return -value
            else:
                raise ToyLangException(
                    "[line %d] '-' operator can only be applied to numeric values" % expression.operator.line)

    def visit_variable_expression(self, expression):
        if expression.name not in self.symbol_table:
            raise ToyLangException("Undefined name '%s'" % expression.name)
        return self.symbol_table[expression.name]

    def visit_binary_expression(self, expression):
        left = self.evaluate(expression.left)
        right = self.evaluate(expression.right)
        op_type = expression.operator.type

        if op_type == TokenType.PLUS:
            if isinstance(left, str) and isinstance(right, str):
                return left + right
            elif isinstance(left, str):
                return left + str(right).lower()
            elif isinstance(right, str):
                return str(left).lower() + right
            elif self.is_number(left) and self.is_number(right):
                return left + right
            elif self.is_number(left) or self.is_number(right):
                # one of them is boolean
                raise ToyLangException(
                    "[line %d] '+' not supported between boolean and numeric values" %
                    expression.operator.line)
            else:
                # both are boolean values
                raise ToyLangException(
                    "[line %d] '+' not supported between two boolean values" %
                    expression.operator.line)
        if op_type in [TokenType.MINUS, TokenType.STAR, TokenType.SLASH, TokenType.PERCENT]:
            if not self.is_number(left) or not self.is_number(right):
                raise ToyLangException(
                    "[line %d] '%s' is supported only between numeric values" %
                    (expression.operator.line, expression.operator.lexeme))
            if op_type == TokenType.MINUS:
                return left - right
            elif op_type == TokenType.STAR:
                return left * right
            elif op_type == TokenType.SLASH:
                if right == 0:
                    raise ToyLangException("[line %d] Division by zero" % expression.operator.line)
                return left / right
            elif op_type == TokenType.PERCENT:
                return left % right
        elif op_type == TokenType.EQUAL_EQUAL:
            return left == right
        elif op_type == TokenType.NOT_EQUAL:
            return left != right
        elif op_type == TokenType.AND or op_type == TokenType.OR:
            if not isinstance(left, bool) or not isinstance(right, bool):
                raise ToyLangException("[line %d] '%s' is supported only between boolean values"
                                       % (expression.operator.line, expression.operator.lexeme))
            if op_type == TokenType.AND:
                return left and right
            else:
                return left or right
        elif op_type in [TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LOWER, TokenType.LOWER_EQUAL]:
            if self.is_number(left) and self.is_number(right) or type(left) == type(right):
                if op_type == TokenType.GREATER:
                    return left > right
                elif op_type == TokenType.GREATER_EQUAL:
                    return left >= right
                elif op_type == TokenType.LOWER:
                    return left < right
                else:
                    return left <= right
            else:
                raise ToyLangException("[line %d] '%s' is supported only between instances of the same type"
                                       % (expression.operator.line, expression.operator.lexeme))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('filename')
    args = parser.parse_args()
    try:
        with open(args.filename, 'rt') as source_file:
            code = source_file.read()
        tokens = Scanner(code).scan_tokens()
        statements = Parser(tokens).parse()
        Interpreter(statements).run()
    except FileNotFoundError:
        print('Error: file not found')
    except ToyLangException as e:
        print('Error:', e)


if __name__ == '__main__':
    main()
