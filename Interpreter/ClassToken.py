#====ALL TOKENS====
GETAL   = 'GETAL'     #int
ZWEEF   = 'ZWEEF'     #float
WOORD   = 'WOORD'     #variable
PLUS    = 'PLUS'      # +
MIN     = 'MIN'       # -
KEER    = 'KEER'      # *
DEEL    = 'DEEL'      # /
EOF     = 'EOF'       #EOF

class Token:
    def __init__(self, type: str, value: str) -> None:
        self.type = type
        self.value = value

    def __str__(self) -> str:
        return 'Token({type}, {value})'.format(
            type=self.type,
            value=repr(self.value)
        )
    
    def __repr__(self) -> str:
        return self.__str__()

