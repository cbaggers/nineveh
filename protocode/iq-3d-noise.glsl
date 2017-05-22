//iq's ubiquitous 3d noise
float noise(in vec3 p)
{
    vec3 ip = floor(p), f = fract(p);
	f = f*f*(3.0-2.0*f);
	vec2 uv = (ip.xy+vec2(37.0,17.0)*ip.z) + f.xy;
	vec2 rg = textureLod( iChannel0, (uv+ 0.5)/256.0, 0.0 ).yx;
	return mix(rg.x, rg.y, f.z);
}
