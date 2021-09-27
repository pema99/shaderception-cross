Shader "Unlit/PSL"
{
    Properties
    {
        _Video ("Video", 2D) = "white" {}
        _Camera ("Camera", 2D) = "white" {}
    }
    SubShader
    {
        Pass
        {
            CGPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #include "UnityCG.cginc"

            struct appdata
            {
                float4 vertex : POSITION;
                float2 uv : TEXCOORD0;
            };

            struct v2f
            {
                float4 vertex : SV_POSITION;
                float2 uv : TEXCOORD0;
            };

            v2f vert (appdata v)
            {
                v2f o;
                o.vertex = UnityObjectToClipPos(v.vertex);
                o.uv = v.uv;
                return o;
            }

            sampler2D _Camera;
            sampler2D _Video;

            // === Begin standard library ===
            static float2 globaluv;
            float2 uv()
            {
                return globaluv;
            }

            float2 xy()
            {
                return float2(2.0 * (globaluv - 0.5) * 10.0);
            }
            
            float4 time()
            {
               return _Time;
            }

            float video(float2 uv)
            {
                return tex2D(_Video, uv);
            }
            
            float camera(float2 uv)
            {
                return tex2D(_Camera, uv);
            }

            float4 __cast(float a) { return a.xxxx; }
            float4 __cast(float2 a) { return float4(a, 0, 1); }
            float4 __cast(float3 a) { return float4(a, 1); }
            float4 __cast(float4 a) { return a; }

            #define mod(x,y) (((x)-(y)*floor((x)/(y)))) 
            
            // === Begin user code ===
            __CODE_GOES_HERE__

            float4 frag (v2f i) : SV_Target
            {
                globaluv = i.uv;
                float4 col = __cast(main());
                col.rgb = LinearToGammaSpace(col.rgb);
                return col;
            }
            ENDCG
        }
    }
}
