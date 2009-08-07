.section .data
  s1: .asciz "El nombre de As es: "
  s2: .asciz "S'ha acabat"

.section .text
  .global _provastdio

  _provastdio:
    pushl %ebp            # Preàmbul
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
    pushl %eax                      # número introduït.
    call  _getc
    addl  $4, %esp

    leal  -8(%ebp), %eax  # put(c); # imprimeix el CR, amb l'efecte
    pushl %eax                      # de saltar una línia
    call  _putc
    addl  $4, %esp

    leal  -8(%ebp), %eax  # get(c); # llegeix un caràcter
    pushl %eax
    call  _getc
    addl  $4, %esp

    leal  -8(%ebp), %eax  # put(c); # l'imprimeix
    pushl %eax
    call  _putc
    addl  $4, %esp

    leal  -8(%ebp), %eax  # get(c); # llegeix un caràcter
    pushl %eax
    call  _getc
    addl  $4, %esp

    leal  -8(%ebp), %eax  # put(c); # l'imprimeix
    pushl %eax
    call  _putc
    addl  $4, %esp

    leal  -8(%ebp), %eax  # get(c); # Si només s'han introduït dos 
    pushl %eax                      # caràcters i a continuació un retorn,
    call  _getc                     # s'empassa el CR
    addl  $4, %esp

    leal  -8(%ebp), %eax  # put(c); # imprimeix el CR, produïnt un salt
    pushl %eax                      # de línia.
    call  _putc
    addl  $4, %esp

    call _new_line        # new_line

    pushl $s2             # put("S'ha acabat");
    call  _puts
    addl  $4, %esp

    movl %ebp, %esp       # Retorn
    popl %ebp
    ret
