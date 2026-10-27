; The reviewed ABI of every SDL struct the stubs read: size, alignment
; and the offset of each field in use. lib/sdl3/generate.exe turns it
; into static asserts at build time; a newer SDL that moves one fails the
; build and names it. Rewrite with
;   dune exec lib/sdl3/generate.exe -- accept
; and review the diff. Never edit by hand.
((SDL_DialogFileFilter 16 8 ((name 0) (pattern 8)))
 (SDL_DropEvent 48 8 ((type 0) (data 40) (x 20) (y 24)))
 (SDL_Event 128 8 ())
 (SDL_KeyboardEvent 40 8 ((type 0) (key 28) (scancode 24) (mod 32) (down 36) (repeat 37)))
 (SDL_MouseButtonEvent 40 8 ((type 0) (button 24) (down 25) (x 28) (y 32)))
 (SDL_MouseMotionEvent 48 8 ((type 0) (x 28) (y 32) (xrel 36) (yrel 40)))
 (SDL_MouseWheelEvent 56 8 ((type 0) (x 24) (y 28) (direction 32) (mouse_x 36) (mouse_y 40) (integer_x 44) (integer_y 48)))
 (SDL_PinchFingerEvent 24 8 ((type 0) (scale 16)))
 (SDL_Surface 48 8 ((w 8) (h 12) (pixels 24) (pitch 16)))
 (SDL_TextEditingEvent 40 8 ((type 0) (text 24) (start 32) (length 36)))
 (SDL_TextInputEvent 32 8 ((type 0) (text 24)))
 (SDL_WindowEvent 32 8 ((type 0) (data1 20) (data2 24))))
