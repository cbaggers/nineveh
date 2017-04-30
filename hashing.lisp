(in-package :nineveh)

;; Credit: IQ
(defun-g hash-0 ((n :float))
  (fract (* (sin n) (glsl-expr "753.5453123" :float))))

;; float hash1( vec2 p )
;; {
;;     p  = 50.0*fract( p*0.3183099 );
;;     return fract( p.x*p.y*(p.x+p.y) );
;; }

;; float hash1( float n )
;; {
;;     return fract( n*17.0*fract( n*0.3183099 ) );
;; }

;; vec2 hash2( float n ) { return fract(sin(vec2(n,n+1.0))*vec2(43758.5453123,22578.1459123)); }

;; vec2 hash2( vec2 p )
;; {
;;     const vec2 k = vec2( 0.3183099, 0.3678794 );
;;     p = p*k + k.yx;
;;     return fract( 16.0 * k*fract( p.x*p.y*(p.x+p.y)) );
;; }
