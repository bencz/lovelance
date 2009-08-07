#
# Llibreria d'operacions d'entrada-sortida bàsiques
#                     stdio.s
#


.section .data
   LP_INT: .asciz "%d"
   LP_CHR: .asciz "%c"
   L_NL:   .asciz "\n" 

.section .text
  .globl _putc
  .globl _puti 
  .globl _puts
  .globl _new_line

  .globl _geti
  .globl _getc
  .globl _getcc

  _puti:                     # procedure puti(n: in integer);
      pushl  %ebp
      movl   %esp, %ebp

      subl   $8, %esp        # Manies del C
      movl   8(%ebp), %eax   # %eax:= adr. de n
      pushl  (%eax)          # 2on paràmetre: valor de n
      pushl  $LP_INT         # 1er paràmetre: adr. de "%d"
      call   _printf         
      addl   $16, %esp       # 8 pels dos paràmetres + 8 de manies.

      movl   %ebp, %esp
      popl   %ebp
      ret

  _putc:                     # procedure putc(c: in character);
      pushl  %ebp
      movl   %esp, %ebp

      subl   $8, %esp        # Manies del C
      movl   8(%ebp), %eax   # %eax:= adr. de c
      pushl  (%eax)          # 2on paràmetre: valor de c
      pushl  $LP_CHR         # 1er paràmetre: adr. de "%c"
      call   _printf         
      addl   $16, %esp       # 8 pels dos paràmetres + 8 de manies.

      movl   %ebp, %esp
      popl   %ebp
      ret

  _puts:                     # procedure puts(s: in string);
      pushl  %ebp
      movl   %esp, %ebp

      subl   $12, %esp       # Manies del C
      pushl  8(%ebp)         # paràmetre (únic): adr. de "%d"
      call   _printf         
      addl   $16, %esp       # 4 pel paràmetre + 12 de manies.

      movl   %ebp, %esp
      popl   %ebp
      ret

  _new_line:                 # procedure new_line;
      pushl  %ebp
      movl   %esp, %ebp

      subl   $12, %esp       # Manies del C
      pushl  $L_NL           # paràmetre (únic): adr. de "\n"
      call   _printf
      addl   $16, %esp       # 4 pel paràmetre + 12 de manies.

      movl   %ebp, %esp
      popl   %ebp
      ret

  _geti:                     # procedure geti(n: out integer);
      pushl  %ebp
      movl   %esp, %ebp

      subl   $8, %esp        # Manies del C
      pushl  8(%ebp)         # 2on paràmetre: adr. de n
      pushl  $LP_INT         # 1er paràmetre: adr. de "%d"
      call   _scanf
      addl   $16, %esp       # 8 pels dos paràmetres + 8 de manies.

      movl   %ebp, %esp
      popl   %ebp
      ret

  _getc:                     # procedure geti(c: out character);
      pushl  %ebp            # per tipus character de 4 bytes
      movl   %esp, %ebp

      movl   8(%ebp), %eax   # %eax:= adr. c
      xor    %ebx, %ebx      # %ebx:= nul (4 bytes)
      movl   %ebx, (%eax)    # c:= nul (4 bytes)

      subl   $8, %esp        # Manies del C
      pushl  %eax            # 2on paràmetre: adr. de c
      pushl  $LP_CHR         # 1er paràmetre: adr. de "%c"
      call   _scanf          # sobreescr. el 1er dels 4 bytes posats a 0
      addl   $16, %esp       # 8 pels dos paràmetres + 8 de manies.

      movl   %ebp, %esp
      popl   %ebp
      ret

  _getcc:                    # procedure getcc(c: out character);                
      pushl  %ebp            # per tipus character de 1 byte
      movl   %esp, %ebp

      subl   $8, %esp        # Manies del C
      pushl  8(%ebp)         # 2on paràmetre: adr. de c
      pushl  $LP_CHR         # 1er paràmetre: adr. de "%c"
      call   _scanf          # c:= caràcter llegit (1 byte)
      addl   $16, %esp       # 8 pels dos paràmetres + 8 de manies.

      movl   %ebp, %esp
      popl   %ebp
      ret
