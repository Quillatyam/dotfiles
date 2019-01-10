function fop --description 'Fish 1password login'
    set -lx token (op signin $OP_SHORT_NAME --output=raw)
    set -gx "OP_SESSION_$OP_SHORT_NAME" $token
end
