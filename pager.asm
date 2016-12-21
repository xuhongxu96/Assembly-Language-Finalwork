;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 文件：   pager.asm                                       ;
; 作者：   xuhongxu.com 改编自汇编程序设计大作业           ;
; 日期：   2016.12.20                                      ;
; 修改：   2016.12.21                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PAGE        60, 132
.MODEL      SMALL

INCLUDE     DOS.INC
INCLUDE     BIOS.INC

.DATA

EXTRN       statatr:BYTE,   scrnatr:BYTE,   sbuffer:WORD,   pbuffer:WORD
EXTRN       fsize:WORD,     cell:WORD,      statline:BYTE,  linenum:WORD
EXTRN       rows:WORD,      vidadr:WORD,    cga:BYTE,       findline:BYTE
EXTRN       findstat:BYTE,  statline_l:WORD,findstr:BYTE,   findlen:WORD
EXTRN       matchn:WORD,    findmatch:BYTE

.CODE
PUBLIC      Pager, isEGA, ShowFind, ShowKey

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 程序：   Pager
; 功能：   显示状态栏和文本行
; 参数：   栈 - 滚动的行数n
; 输出：   无

Pager       PROC
            push    bp
            mov     bp, sp
            
            mov     es, sbuffer             ; es = sbuffer 缓冲段基址
            mov     di, pbuffer             ; di = pbuffer 缓冲偏移量
            
            mov     cx, [bp + 4]            ; 栈参数n


            mov     ax, 10                  ; 搜索换行符 Ascii 10 (LF: Linefeed)
                                            ; GoBack/GoForwd滚动时会涉及换行符搜索
                                            
            or      cx, cx                  ; 判断滚动方向
            jg      forward
            jl      backward
            jmp     SHORT show              ; 不滚动，直接显示
            
backward:   call    GoBack
            jmp     SHORT show
forward:    call    GoForwd

; 显示

show:       cld
            push    di
            push    es
            push    ds
            pop     es                      ; DS->ES
            
; 调用子程序：BinToStr (linenum, OFFSET statline[7]) -> ax
; 将行号转换为文本，长度存入ax

            push    linenum                 ; 参数1
            mov     ax, OFFSET statline[7]  ; 参数2
            push    ax
            call    BinToStr                ; 二进制到文本
            
; 初始化状态栏填充文本
            
            mov     cx, statline_l          ; 行号的空位数
            sub     cx, ax                  ; 填充空格数cx = 空位数 - 行号文本长度
            mov     al, " "
            rep     stosb                   ; 填充空格
            pop     es
            
            mov     bl, statatr             ; 加载状态栏配色
            mov     BYTE PTR cell[1], bl
            
; 调用子程序：CellWrt (ds, OFFSET statline, 0, cell)
; 指定配色写入第0行，即状态栏
            
            push    ds                      ; 参数1
            mov     ax, OFFSET statline     ; 参数2
            push    ax
            sub     ax, ax                  ; 参数3
            push    ax
            push    cell                    ; 参数4
            call    CellWrt
            
; 初始化内容填充

            pop     di
            mov     bl, scrnatr             ; 加载内容配色
            mov     BYTE PTR cell[1], bl
            
            mov     si, di                  ; 缓冲偏移量
            mov     cx, rows                ; cx = 行数

show1:      mov     bx, rows                ; bx = 行数
            inc     bx                      ; bx++
            sub     bx, cx                  ; bx -= cx 即当前行
            push    cx                      ; 保存行数
            
; 调用子程序：CellWrt (sbuffer, pbuffer, line, cell) -> ax
; 指定配色，将缓冲区内容写入当前行，返回写入后的缓冲偏移量

            push    sbuffer                 ; 参数1
            push    si                      ; 参数2
            push    bx                      ; 参数3
            push    cell                    ; 参数4
            call    CellWrt

            pop     cx                      ; 还原行数
            mov     si, ax                  ; 更新缓冲偏移量
            
            cmp     ax, fsize               ; 是否读到文件尾
            jae     fillout                 ; 是，跳到fillout，用空格填充剩余
            loop    show1                   ; 否则，继续写入下一行，cx--
            jmp     SHORT pagedone          ; 结束则跳到pagedone
            
; 空格填充剩余

fillout:    dec     cx                      ; 计算剩余行数
            jcxz    pagedone                ; 为0，结束，跳到pagedone
            
            ; 列数 * 剩余行数
            
            mov     al, 80                  
            mul     cl
            
; 调用子程序：CellFil (sbuffer, count, cell)
; 指定配色，缓冲区填充指定数目空格
            
            push    sbuffer                 ; 参数1
            push    ax                      ; 参数2
            push    cell                    ; 参数3
            call    CellFil
  
pagedone:   @SetCurPos  0, 43               ; 移除光标
            mov     findstat, 0             ; 搜索栏不可见
            pop     bp
            ret     2
Pager       ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
; 程序：   CellWrt (segment, offset, line, cell)
; 功能：   写入一行
; 参数：   栈 - 1. 缓冲所在段；2. 缓冲偏移量；3. 行号；4. 配色
; 输出：   ax - 写入后的缓冲偏移量

CellWrt     PROC
            push    bp
            mov     bp, sp
            
            push    ds
            sub     dx, dx
            cmp     cga, 1                  ; CGA?
            jne     noscan                  ; 不是CGA，noscan
            mov     dx, 03DAh               ; 加载端口
            
noscan:     mov     es, vidadr              ; 加载屏幕缓冲段
            mov     ds, [bp + 10]           ; ds = 缓冲所在段
            mov     si, [bp + 8]            ; si = 缓冲偏移量
            mov     cx, 80                  ; 列数
            mov     ax, [bp + 6]            ; 行号
            mov     bx, 80 * 2              ; 每行字节数
            mul     bl                      ; 行号 * 每行字节数 = 起始字节偏移
            mov     di, ax                  ; di = 起始字节偏移
            mov     bx, di                  ; bx = 起始字节偏移
            mov     ax, [bp + 4]            ; 配色
movechar:   lodsb                           ; 从文件缓冲区（ds:si）取字符到al
            cmp     al, 13                  ; 是否为回车符
            je      fillspc                 ; 是回车符，该行内容结束，跳到fillspc填充空格
            cmp     al, 9                   ; 是否为Tab
            jne     notab                   ; 不是Tab，跳到notab

; 调用子程序：FillTab
; 填充Tab空格

            call    FillTab           
            
            jcxz    nextline                ; 如果超过列数，结束本行，显示下一行
            jmp     SHORT movechar          ; 否则，继续显示本行下一字符
            
notab:      or      dx, dx                  ; CGA?
            je      notab2                  ; 不是CGA，跳到notab2
            
; 调用子程序：Retrace
; 是CGA时，使用Retrace

            call    Retrace
            
            loop    movechar                ; 继续显示本行下一字符
            jmp     SHORT nextline          ; 显示下一行

notab2:     stosw                           ; 将字符ax存到屏幕缓冲区（es:di）     
            loop    movechar                ; 继续显示本行下一字符
            jmp     SHORT nextline          ; 显示下一行
            
fillspc:    mov     al, " "                 ; 该行结束，为剩余列填充空格
            or      dx, dx                  ; CGA?
            je      space2                  ; 不是CGA，跳到space2
                        
; 调用子程序：Retrace
; 是CGA时，使用Retrace

space1:     call    Retrace
            loop    space1                  ; 继续填充空格
            inc     si                      ; 缓冲偏移量++
            jmp     SHORT exit              ; 结束
            
space2:     rep     stosw                   ; 写空格
            inc     si                      ; 下一位置
            jmp     SHORT exit              ; 结束

nextline:   mov     ah, 10                  ; 搜索换行符
chklf:      lodsb                           ; 文件缓冲区（ds:si）读取字符到al
            cmp     al, ah                  ; 比较是否是换行符
            loopne  chklf                   ; 不是就继续找
exit:       mov     ax, si                  ; 返回ax = 写入后的偏移量
            pop     ds
            pop     bp
            ret     8
CellWrt     ENDP
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
; 程序：   CellFil (segment, count, cell)
; 功能：   向缓冲区填充指定指定数目字符
; 参数：   栈 - 1. 缓冲所在段；2. 数目；3. 配色和字符（高位和低位）
; 输出：   ax - 写入后的缓冲偏移量           
            
CellFil     PROC
            push    bp
            mov     bp, sp
            
            push    ds
            sub     dx, dx
            cmp     cga, 1                  ; CGA?
            jne     noscan2                 ; 不是CGA，noscan
            mov     dx, 03DAh               ; 加载端口

noscan2:    mov     es, vidadr              ; 加载屏幕缓冲段
            mov     ds, [bp + 8]            ; 缓冲所在段
            mov     cx, [bp + 6]            ; 填充的数目
            mov     ax, [bp + 4]            ; 配色和字符
            or      dx, dx                  ; CGA?
            je      fillem2                 ; 不是，跳到fillem2

; 调用子程序：Retrace
; 是CGA时，使用Retrace

fillem1:    call    Retrace

            loop    fillem1                 ; 重复填充
            jmp     SHORT filled            ; 填充完毕，结束跳到filled
fillem2:    rep     stosw                   ; 写ax到屏幕缓冲区（es:di）
filled:     pop     ds
            pop     bp
            ret     6
CellFil     ENDP
   
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
; 程序：   FillTab
; 功能：   填充Tab
; 参数：   bx - 指向行首位置，di - 指向当前位置，cx - 剩余列数
; 输出：   cx - 剩余列数

FillTab     PROC
            push    bx
            push    cx
            
            ; 计算行内偏移（除以2是因为一个显示的字符占两个字节）存入bx
            sub     bx, di                  ; bx = bx - di
            neg     bx                      ; bx = -bx
            shr     bx, 1                   ; bx /= 2
            
            mov     cx, 4                   ; 默认Tab为4个空格
            and     bx, 3                   ; bx = bx mod 4，即Tab占用位数
            sub     cx, bx                  ; cx -= bx，即Tab剩余空位
            mov     bx, cx                  ; 存储cx到bx
            
            mov     al, " "                 ; 空格字符
            or      dx, dx                  ; CGA?
            je      tabem2                  ; 不是，跳到tabem2

; 调用子程序：Retrace
; 是CGA时，使用Retrace

tabem1:     call    Retrace
            loop    tabem1                  ; 重复填充
            jmp     SHORT tabbed            ; 填充完毕，结束跳到tabbed
tabem2:     rep     stosw                   ; 写ax到屏幕缓冲区（es:di）
tabbed:     pop     cx                      ; cx此时是列数
            sub     cx, bx                  ; cx -= bx
            jns     nomore                  ; 如果无符号，说明cx > bx，跳到nomore
            sub     cx, cx                  ; 否则，cx为负，该列已经到头了，清空cx
nomore:     pop     bx
            ret
FillTab     ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
; 程序：   GoBack
; 功能：   向上搜索缓冲
; 参数：   cx - 行数（向上所以是负数），es:di - 缓冲位置
; 输出：   无

GoBack      PROC
            std                             ; set df = 1，向上
            neg     cx                      ; cx变正
            mov     dx, cx                  ; 保存cx到dx
            inc     cx                      ; 加一步
            or      di, di                  ; 判断是否为文件头（di=0）
            je      exback                  ; di = 0，是文件头，跳到exback
findb:      push    cx                      ; 不是文件头，保存cx
            mov     cx, 0FFh                ; 查找0FFh（最大文本长度）个字符
            cmp     cx, di                  ; 是否超出缓冲位置
            jl      notnear                 ; 没有，那继续
            mov     cx, di                  ; 超出了，修改加载字符数为
notnear:    repne   scasb                   ; 找到前一个LF
            jcxz    atstart                 ; 如果没找到，就去开头
            pop     cx                      ; 还原行数
            loop    findb                   ; 再上一行
            cmp     linenum, 0FFFFh         ; EOF?
            jne     notend                  ; 没有，继续
            add     di, 2                   ; 跳过CR/LF
            mov     pbuffer, di             ; 存储位置
            call    EndCount                ; 计算行号
            ret
notend:     sub     linenum, dx             ; 计算行号
            jg      positive
            mov     linenum, 1              ; 行号为负数，设为1
positive:   add     di, 2                   ; 跳过CR/LF
            mov     pbuffer, di             ; 存储位置
            ret
atstart:    pop     cx
            sub     di, di                  ; di = 0，到文件开头
            mov     linenum, 1              ; 设行为1
            mov     pbuffer, di             ; 存储位置
exback:     ret
GoBack      ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
; 程序：   GoForwd
; 功能：   向下搜索缓冲
; 参数：   cx - 行数（向上所以是负数），es:di - 缓冲位置
; 输出：   无

GoForwd     PROC
            cld                             ; set df = 0，向下
            mov     dx, cx                  ; 保存cx到dx
findf:      push    cx                      ; 保存cx
            mov     cx, 0FFh                ; 查找0FFh（最大文本长度）个字符
            repne   scasb                   ; 找到下一个LF
            jcxz    atend                   ; 没找到，已经在文件尾
            cmp     di, fsize               ; 是否超出结尾
            jae     atend                   ; 超出，已经在文件尾
            pop     cx
            loop    findf                   ; 继续下一行
            add     linenum, dx             ; 计算行号
            mov     pbuffer, di             ; 存储位置
            ret
atend:      pop     cx                      ; 已在文件尾，不做事情
            mov     di, pbuffer             ; 还原位置
            ret
GoForwd     ENDP
            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
; 程序：   EndCount
; 功能：   向上计算行数
; 参数：   es:di - 缓冲位置
; 输出：   无            

EndCount    PROC
            push    di
            
            mov     al, 13                  ; 搜索CR
            mov     linenum, 0              ; 从0行开始
            
findstrt:   inc     linenum                 ; 下一行
            mov     cx, 0FFh                ; 查找0FFh（最大文本长度）个字符
            cmp     cx, di                  ; 超出缓冲位置
            jl      notnear2                ; 没超出，继续
            mov     cx, di                  ; 超出，只搜到缓冲位置
notnear2:   repne   scasb                   ; 向上找CR
            jcxz    found                   ; 没找到CR，那就一直到开头
            jmp     SHORT findstrt          ; 找到，继续上一行
found:      pop     di
            ret
EndCount    ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
; 程序：   isEGA
; 功能：   判断EGA是否激活
; 参数：   无
; 输出：   al - 0为激活（是EGA），未激活则返回屏幕行数

isEGA       PROC
            push    bp
            push    es
            mov     ah, 12h                 ; 获取 EGA 状态
            mov     bl, 10h
            sub     cx, cx                  ; 清除状态位
            int     10h
            sub     ax, ax                  ; 清空返回值
            jcxz    noega                   ; 如果状态位仍为0，没有EGA
            
            mov     es, ax                  ; es = 0
            test    BYTE PTR es:[487h], 1000b   ; 测试激活位
            jnz     noega                   ; 如果不为0，未激活
            mov     ax, 1130h               ; 获取EGA信息
            int     10h
            mov     al, dl                  ; 返回行数
            cbw
noega:      pop     es
            pop     bp
            ret
isEGA       ENDP
            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
; 程序：   BinToStr
; 功能：   转换整数到文本
; 参数：   栈 - 1. 欲转换数值，2. 写入地址
; 输出：   ax - 字符数           

BinToStr    PROC
            push    bp
            mov     bp, sp
            mov     ax, [bp + 6]            ; 参数1
            mov     di, [bp + 4]            ; 参数2
            
            sub     cx, cx                  ; 清除cx
            mov     bx, 10                  ; bx = 10
getdigit:   sub     dx, dx                  ; dx = 0
            div     bx                      ; 除以10
            add     dl, "0"                 ; 余数转换为Ascii
            push    dx                      ; 字符存到栈
            or      ax, ax                  ; 商是否为0
            loopnz  getdigit                ; 不为0，继续转换
            
            neg     cx                      ; cx = -cx，即长度
            mov     dx, cx
putdigit:   pop     ax                      ; 得到字符
            stosb                           ; 存储字符到指定地址
            loop    putdigit
            mov     ax, dx                  ; 返回字符数
            
            pop     bp
            ret     4
BinToStr    ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
; 程序：   Retrace
; 功能：   CGA模式下写字符
; 参数：   es:di - 屏幕缓冲偏移，ax - 配色和字符
; 输出：   无

Retrace     PROC
            push    bx
            mov     bx, ax                  ; 保存字符
lscan2:     in      al, dx                  ; 找到端口
            shr     al, 1                   ; 直到低位
            jc      lscan2
            cli
hscan2:     in      al, dx                  ; 保存字符
            shr     al, 1                   ; 直到低位
            jnc     hscan2
            mov     ax, bx                  ; 还原并写入
            stosw
            sti
            pop     bx
            ret
Retrace     ENDP
         

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 程序：   ShowFind
; 功能：   切换搜索栏
; 参数：   无
; 输出：   无

ShowFind    PROC
            push    bp
            mov     bp, sp
            
            push    dx
            
            mov     dl, findstat            ; 搜索栏是否可见
            or      dl, dl
            je      tofind                  ; 为0不可见，则切换为可见
            mov     findstat, 0             ; 否则，置为0
            
; 调用子程序：Pager (0)
; 刷新显示，以隐藏搜索栏

            sub     dx, dx
            push    dx
            call    Pager
            
            @SetCurPos  0, 43               ; 隐藏光标
            
            pop     dx
            pop     bp
            ret
tofind:
            mov     findlen, 0              ; 重置搜索缓冲
            mov     matchn, 0
            call    ShowMatch
            
            ; 初始化搜索栏填充文本
            
            push    es
            push    ds
            pop     es
            mov     di, OFFSET findline + 12
            mov     cx, 45                  ; 行号的空位数
            mov     al, " "
            rep     stosb                   ; 填充空格
            pop     es

; 调用子程序：CellWrt (ds, OFFSET findline, rows, cell)
; 指定配色，填充搜索栏内容
    
            push    ds                      ; 参数1
            mov     dx, OFFSET findline     ; 参数2
            push    dx
            mov     dx, rows                ; 参数3
            push    dx
            mov     dl, statatr             ; 参数4
            mov     BYTE PTR cell[1], dl
            push    cell
            call    CellWrt
            
            mov     dh, BYTE PTR rows
            @SetCurPos      12              ; 显示光标
            mov     findstat, 1             ; 设置搜索栏可见
            
            pop     dx
            pop     bp
            ret
ShowFind    ENDP

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 程序：   ShowKey
; 功能：   显示输入的字符
; 参数：   al - Ascii
; 输出：   无

ShowKey     PROC
            push    cx
            push    dx
            
            mov     dl, findstat            ; 搜索栏是否可见
            or      dl, dl
            je      endshow                 ; 为0不可见，返回
            
            cmp     al, 8                   ; 退格键？
            jne     normalkey               ; 不是，跳到正常按键
            mov     si, findlen             ; 退格，判断长度
            or      si, si
            je      endshow                 ; 长度=0，结束
            dec     findlen                 ; 文本长度--
            dec     si
            jmp     showk
normalkey:  
            cmp     al, 32
            jl      endshow
            mov     si, findlen             ; 搜索文本长度
            cmp     si, 44
            jg      endshow
            mov     findstr[si], al         ; 加入新字符
            inc     si                      ; 长度++
            mov     findlen, si             ; 更新长度
showk:
            mov     dh, BYTE PTR rows
            mov     dl, BYTE PTR findlen
            add     dl, 12
            @SetCurPos                      ; 更新显示光标
            
            push    es
            push    ds
            pop     es                      ; ds->es
            
            push    si                      ; 保存长度
            mov     cx, si                  ; cx = 长度
            mov     si, OFFSET findstr      ; si为搜索文本起始
            mov     di, OFFSET findline + 12    ; di为搜索栏的填空起始
            
            rep     movsb                   ; 循环复制

            ; 初始化搜索栏填充文本
            
            pop     di                      ; di取出长度
            mov     ax, di                  ; 存入ax
            add     di, OFFSET findline + 12    ; di为空格起始
            mov     cx, 45                  ; 行号的空位数
            sub     cx, ax                  ; 填充空格数cx = 空位数 - 行号文本长度
            mov     al, " "
            rep     stosb                   ; 填充空格
            
            pop     es                      ; 恢复es
            
            
; 调用子程序：FindString

            call    FindString
            
            
; 调用子程序：CellWrt (ds, OFFSET findline, rows, cell)
; 指定配色，填充搜索栏内容
    
            push    ds                      ; 参数1
            mov     dx, OFFSET findline     ; 参数2
            push    dx
            mov     dx, rows                ; 参数3
            push    dx
            mov     dl, statatr             ; 参数4
            mov     BYTE PTR cell[1], dl
            push    cell
            call    CellWrt
endshow:           
            pop     dx
            pop     cx
            ret
ShowKey     ENDP
                
                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 程序：   FindString
; 功能：   搜索文本
; 参数：   无
; 输出：   无   

FindString  PROC
            
            mov     matchn, 0
            mov     ax, fsize
            mov     bx, findlen
            sub     ax, bx
            jb      findfail
            
            mov     dx, 0FFFFh
            
findnext:   cmp     dx, ax
            je      findfail
            inc     dx
            
            cld
            push    es
            mov     es, sbuffer
            mov     di, dx
            mov     si, OFFSET findstr
            
            mov     cx, bx
            repe    cmpsb
            
            pop     es
            
            jnz     findnext
            
            inc     matchn
            jmp     findnext
            
findfail:   
            call    ShowMatch

            ret

FindString  ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 程序：   ShowMatch
; 功能：   显示匹配数目
; 参数：   无
; 输出：   无   

ShowMatch   PROC

; 调用子程序：BinToStr (matchn, OFFSET findmatch) -> ax
; 将匹配数目转换为文本，长度存入ax
            push    es
            push    ds
            pop     es
            push    matchn                  ; 参数1
            mov     ax, OFFSET findmatch    ; 参数2
            push    ax
            call    BinToStr                ; 二进制到文本
              
            mov     cx, 7                   ; 匹配数目的空位数
            sub     cx, ax                  ; 填充空格数cx = 空位数 - 行号文本长度
            mov     al, " "
            rep     stosb                   ; 填充空格
            pop     es
            ret
            
ShowMatch   ENDP



            END












            
            
            
            