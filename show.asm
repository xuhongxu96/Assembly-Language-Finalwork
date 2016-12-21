;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 文件：   show.asm                                        ;
; 作者：   xuhongxu.com 改编自汇编程序设计大作业           ;
; 日期：   2016.12.21                                      ;
; 修改：   2016.12.21                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PAGE
TITLE       SHOW

DOSSEG
.MODEL      SMALL

INCLUDE     DOS.INC
INCLUDE     BIOS.INC

.STACK      100h

.DATA

; 状态栏

PUBLIC      statline, linenum, statline_l
statline    DB      " Line:      "                      ; 状态栏行号提示
statfile    DB      " File:               "             ; 状态栏文件名提示
stathelp    DB      " Quit:Esc  Etr:Fnd  Move:  PGUP PGDN HOME END"   ; 状态栏帮助提示
statline_l  DW      statfile - statline - 7             ; 状态栏行号空位长
linenum     DW      1                                   ; 行号

; 搜索栏

PUBLIC      findline, findstat, findstr, findlen
findstat    DB      0                                   ; 搜索栏是否可见
findline    DB      " Find Text:", 50 dup(" "), "<< 0       matched "             ; 搜索栏提示
findstr     DW      60 dup (0)                          ; 搜索缓冲区
findlen     DB      0                                   ; 搜索缓冲大小

; 屏幕显示控制变量

PUBLIC      cell, rows, columns, vidadr, statatr, scrnatr, cga
cell        LABEL   WORD                    ; 屏幕显示单元（字符和配色）
char        DB      " "                     ; | 字符：初始为空格
attr        DB      ?                       ; | 属性

columns     EQU     80                      ; 列数
rows        DW      24                      ; 行数
mode        DB      ?                       ; 显示模式
pag         DB      ?                       ; 显示页
newvid      DB      0                       ; 显示交换标识
cga         DB      1                       ; CGA标识（默认真）

vidadr      DW      0B800h                  ; 显示缓冲地址（默认CGA）
mono        EQU     0B000h                  ; 单色地址

hltatr      EQU     047h                    ; 高亮配色：红底白字
statatr     DB      030h                    ; 状态栏默认配色：蓝绿底黑字
bwstat      EQU     070h                    ; 白底黑字配色
scrnatr     DB      017h                    ; 屏幕默认配色：蓝底白字
bwscrn      EQU     007h                    ; 黑底白字配色

; 缓冲和文件控制变量

PUBLIC      buffer, pbuffer, sbuffer, fsize, namebuf
buffer      LABEL   DWORD
pbuffer     DW      0                       ; 缓冲偏移量
sbuffer     DW      ?                       ; 缓冲段基址
lbuffer     DW      ?                       ; 缓冲大小
fhandle     DW      ?                       ; 文件句柄
fsize       DW      ?                       ; 文件大小

prompt      DB      13, 10, 13, 10, "Enter filename: $"
prompt2     DB      13, 10, "File problem. Try again? $"
namebuf     DB      66, ?
filename    DB      66 dup(0)               ; 文件名缓冲

err1        DB      13, 10, "Must have DOS 2.0 or higher", 13, 10, "$"
err2        DB      13, 10, "File too big", 13, 10, "$"

; 函数调用表

exkeys      DB      71, 72, 73, 79, 80, 81  ; 扩展键盘扫描码
lexkeys     EQU     $ - exkeys              ; 键表长
extable     DW      homek
            DW      upk 
            DW      pgupk
            DW      endk
            DW      downk
            DW      pgdnk
            DW      nonek
            
.CODE

EXTRN       pager:PROC, isEGA:PROC, ShowFind:PROC, ShowKey:PROC

start:      mov     ax, @DATA                   ; 初始化数据段为DATA
            mov     ds, ax
            
            cli                                 ; 关闭中断
            mov     ss, ax                      ; 初始化堆栈段为DATA
            mov     sp, OFFSET STACK            ; 初始化堆栈指针
            sti                                 ; 开启中断
            
; 调整内存分配
            
            mov     bx, sp
            mov     cl, 4
            shr     bx, cl
            add     ax, bx
            mov     bx, es
            sub     ax, bx
            @ModBlok    ax

; 为文件缓冲分配动态内存空间
            
            @GetBlok    0FFFh                   ; 尝试分配64K
            mov     sbuffer, ax                 ; 存储缓冲的段基址
            mov     lbuffer, bx                 ; 存储实际分配大小
            
; 检查DOS版本

            @GetVer
            cmp     al, 2                       ; 比较2.0
            jge     video                       ; >= 2.0，可以
            @DispStr    err1                    ; 否则报错
            int     20h
            
; 设置显示模式

; 调用子程序：isEGA -> al
; 判断EGA是否激活，未激活（非EGA）返回屏幕行数，否则返回0

video:      call    isEGA                       ; 判断EGA还是VGA
            or      ax, ax                      ; 若为0，则是CGA（或MA）
            je      modechk                     ; 为0，跳到modechk
            mov     rows, ax                    ; 否则加载行数
            dec     cga                         ; 不是CGA
            
modechk:    @GetMode                            ; 获取显示模式
            mov     mode, al                    ; 存储显示模式
            mov     pag, bh                     ; 存储页
            mov     dl, al                      ; 复制
            cmp     dl, 7                       ; 7模式（单色）
            je      loadmono                    ; 是，跳转到loadmono
            cmp     dl, 15                      ; 15模式（单色）
            jne     graphchk                    ; 不是，跳转到graphchk
loadmono:   mov     vidadr, mono                ; 加载单色地址
            mov     statatr, bwstat             ; 更换配色为单色方案
            mov     scrnatr, bwscrn
            dec     cga                         ; 不是CGA
            cmp     al, 15                      ; 15模式
            jne     cmdchk                      ; 不是15而是7，结束，跳到cmdchk
            mov     dl, 7                       ; 否则，设模式为7
            jmp     SHORT chmod
graphchk:   cmp     dl, 7                       ; 与7比
            jg      color                       ; >7则跳到color
            cmp     dl, 4                       ; 与4比
            jg      bnw                         ; 5, 6一般为黑白
            je      color                       ; 4是彩色
            test    dl, 1                       ; 奇数测试
            jz      bnw                         ; 0, 2是黑白
color:      cmp     dl, 3                       ; 与3比
            je      cmdchk                      ; =3，结束，跳到cmdchk
            mov     dl, 3                       ; 否则，设模式为3
            jmp     SHORT chmod
bnw:        mov     statatr, bwstat             ; 更换配色为单色方案
            mov     scrnatr, bwscrn             ; 屏幕黑底白字配色
            cmp     dl, 2                       ; 与2比
            je      cmdchk                      ; =2，结束，跳到cmdchk
            mov     dl, 2                       ; 否则设置模式为2
chmod:      @SetMode    dl                      ; 设置显示模式
            @SetPage    0                       ; 设置页
            mov     newvid, 1                   ; 设置标识
            
; 打开命令行文件

cmdchk:     mov     bl, es:[80h]                ; 获取长度
            sub     bh, bh
            mov     WORD PTR es:[bx + 81h], 0   ; 添加字符串终止符'\0'
            push    ds
            @OpenFil    82h, 0, es              ; 打开文件
            pop     ds
            jc      getname                     ; 如果错误，重新获取文件名
            mov     fhandle, ax                 ; 否则，保存文件句柄
            push    ds
            @GetFirst   82h, , es               ; 找到文件名
            pop     ds
            jnc     opened                      ; 文件成功打开
            
; 获取文件名

getname:    @DispStr    prompt                  ; 显示提示
            @GetStr     namebuf, 0              ; 获取输入文本
            @OpenFil    filename, 0             ; 打开文件
            jc      badfile                     ; 如果错误，转到badfile
            mov     fhandle, ax                 ; 保存文件句柄
            @GetFirst   filename                ; 找到文件名
            jnc     opened                      ; 文件成功打开

badfile:    @DispStr    prompt2                 ; 打开错误，提示是否再试
            @GetKey     0, 1, 0
            and     al, 11011111b               ; 转换输入为大写
            cmp     al, "Y"                     ; 判断输入是否为Y
            je      getname                     ; 是Y，重试
            jmp     quit                        ; 否则，退出
     
; 复制文件名到状态栏
     
opened:     mov     si, 9Eh                     ; 加载FCB
            mov     di, OFFSET statfile[7]      ; 加载状态栏 文件名
            mov     al, es:[si]                 ; 加载第一个字节
            inc     si
copy:       mov     [di], al                    ; 存储并加载字节，直到0
            inc     di
            mov     al, es:[si]
            inc     si
            or      al, al                      ; 检查是否为0
            loopne  copy                        ; 不是0继续复制

; 检查文件大小

            @GetFilSz   fhandle                 ; 获得文件大小
            or      dx, dx                      ; 比64K大？
            jne     big                         ; 是的，那就太大了
            mov     fsize, ax                   ; 存储文件大小
            mov     cx, 4                       ; 右移四位
            shr     ax, cl
            cmp     ax, lbuffer                 ; 是否比缓冲区大
            jle     fileread                    ; 不大，跳到fileread
big:        @DispStr    err2                    ; 文件过大错误
            @Exit       2
fileread:   push    ds
            @Read   buffer, fsize, fhandle      ; 读取文件
            pop     ds
            jnc     readok                      ; 没有读取错误
            jmp     getname                     ; 否则重新获取文件

; 存储文件大小

readok:     mov     di, ax                      ; 加载文件大小
            push    es
            mov     es, sbuffer                 ; 加载缓冲区段
            std                                 ; set di = 1
            mov     cx, 0FFh                    ; 查找0FFh（最大长度）个字符
            mov     al, 1Ah                     ; 查找EOF（1Ah）
            repne   scasb
            cld
            jcxz    noeof                       ; 如果没有EOF，跳到noeof
            inc     di                          ; 否则存储文件大小
            mov     fsize, di
noeof:      pop     es
            @SetCurPos  0, 43                   ; 移除光标
       
; 调用子程序：Pager (0)       
; 显示第一页

            xor     ax, ax                      ; ax = 0
            push    ax                          ; 参数1
firstpg:    call    Pager

; 处理键盘事件

nextkey:    @GetKey     0, 0, 0                 ; 获取按键
nextkey2:   cmp     al, 0                       ; 是否为空
            je      extended                    ; 如果是，则为扩展键，跳到extended
            cmp     al, 13                      ; 是否为Enter
            jne     nextkey3                    ; 不是，跳到Esc判断
            call    findk                       ; 是Enter，调用findk
            jmp     nextkey
nextkey3:   cmp     al, 27                      ; 是否为Esc
            je      quit                        ; 是Esc，跳到quit
            call    ShowKey                     ; 调用显示key子程序，为搜索栏显示
            jmp     nextkey
quit:       @ClosFil    fhandle                 ; 是Esc，关闭文件
            @FreeBlok   sbuffer                 ; 释放缓冲区
            cmp     newvid, 1                   ; 显示模式是否改变
            jne     thatsall                    ; 没改，跳到thatsall
            @SetMode    mode                    ; 还原显示模式
            @SetPage    pag                     ; 还原页
thatsall:   mov     dx, rows                    ; 加载行数
            xchg    dl, dh                      ; dh = 行数
            mov     cx, dx                      ; cx = dx
            mov     dl, 79                      ; dl = 79
            @Scroll     0                       ; 滚动到新行
            sub     dl, dl
            @SetCurPos                          ; 设置光标
            @Exit       0
extended:   @GetKey     0, 0, 0                 ; 获取扩展码
            push    es
            push    ds
            pop     es
            mov     di, OFFSET exkeys           ; 加载键表的地址和长度
            mov     cx, lexkeys + 1
            repne   scasb                       ; 找到位置
            pop     es
            sub     di, (OFFSET exkeys) + 1     ; 指向键码
            shl     di, 1                       ; 调整指针为字地址
            call    extable[di]
            jmp     nextkey
            
; 键处理

findk:      call    ShowFind
            retn

homek:      mov     pbuffer, 0
            push    pbuffer
            mov     linenum, 1
            call    Pager
            retn

upk:        mov     ax, -1
            push    ax
            call    Pager
            retn

pgupk:      mov     ax, rows
            neg     ax
            push    ax
            call    Pager
            retn

endk:       mov     ax, fsize
            mov     pbuffer, ax
            mov     linenum, -1
            mov     ax, rows
            neg     ax
            push    ax
            call    Pager
            retn

downk:      mov     ax, 1
            push    ax
            call    Pager
            retn
            
pgdnk:      push    rows
            call    Pager
            retn
            
nonek:      retn
            
            END     start























