
#|| - add func for this

 float3 up = float3(0,1,0);
 float3 right = normalize(cross(up,normal));
 up = cross(normal,right);

||#
