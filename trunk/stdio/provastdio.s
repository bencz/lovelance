.section .data
  s1: .asciz "El nombre de As es: "
  s2: .asciz "S'ha acabat"

.section .text
  .global _provastdio

  _provastdio:
    pushl %ebp            # Pre�mbul
    movl  %esp, %ebp
    subl  $8, %esp        # Espai per n i c

    movl  $3, %eax        # n:= 3
    movl  %eax, -4(%ebp)

    movl  $65, %eax       # c:= 'A'
    movl  %eax, -8(%ebp)

    pushl $s1             # put("El nombre de As es: ");
    call  _puts
    addl  $4, %esp

    leal  -4(%ebp), %eax  # put(n);
    pushl %eax
    call  _puti
    addl  $4, %esp

    call _new_line        # new_line

    leal  -8(%ebp), %eax  # put(c);
    pushl %eax
    call  _putc
    addl  $4, %esp

    call _new_line        # new_line

    leal  -4(%ebp), %eax  # get(n);
    pushl %eax
    call  _geti
    addl  $4, %esp

    leal  -4(%ebp), %eax  # put(n);
    pushl %eax
    call  _puti
    addl  $4, %esp

    leal  -8(%ebp), %eax  # get(c); # s'empassa el CR que conclou el
    pushl %eax                      # n�mero introdu�t.
    call  _getc
    addl  $4, %esp

    leal  -8(%ebp), %eax  # put(c); # imprimeix el CR, amb l'efecte
    pushl %eax                      # de saltar una l�nia
    call  _putc
    addl  $4, %esp

    leal  -8(%ebp), %eax  # get(c); # llegeix un car�cter
    pushl %eax
    call  _getc
    addl  $4, %esp

    leal  -8(%ebp), %eax  # put(c); # l'imprimeix
    pushl %eax
    call  _putc
    addl  $4, %esp

    leal  -8(%ebp), %eax  # get(c); # llegeix un car�cter
    pushl %eax
    call  _getc
    addl  $4, %esp

    leal  -8(%ebp), %eax  # put(c); # l'imprimeix
    pushl %eax
    call  _putc
    addl  $4, %esp

    leal  -8(%ebp), %eax  # get(c); # Si nom�s s'han introdu�t dos 
    pushl %eax                      # car�cters i a continuaci� un retorn,
    call  _getc                     # s'empassa el CR
    addl  $4, %esp

    leal  -8(%ebp), %eax  # put(c); # imprimeix el CR, produ�nt un salt
    pushl %eax                      # de l�nia.
    call  _putc
    addl  $4, %esp

    call _new_line        # new_line

    pushl $s2             # put("S'ha acabat");
    call  _puts
    addl  $4, %esp

    movl %ebp, %esp       # Retorn
    popl %ebp
    ret
