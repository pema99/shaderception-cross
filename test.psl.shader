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
            static float2 __globaluv;
            float2 uv()
            {
                return __globaluv;
            }

            float2 xy()
            {
                return float2(2.0 * (__globaluv - 0.5) * 10.0);
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
            
float2 squareComplex(float2 z)
{
    return float2((((z).x) * ((z).x)) - (((z).y) * ((z).y)), (((z).x) * ((z).y)) + (((z).y) * ((z).x)));
}

float squareLength(float2 a)
{
    return (((a).x) * ((a).x)) + (((a).y) * ((a).y));
}

float4 main()
{
    float maxSteps = 20;
    float2 o = uv();
    float2 p = float2((-(2.5) + (((1) - ((-2.5))) * ((o).x))), (-(1.7) + (((1.7) - ((-1.7))) * ((o).y))));
    float2 z = (p) / ((mod((time()).y, 20)) / (3));
    float steps = 0;
    float i = 0;
    while ((i) < (maxSteps))
    {
        if ((squareLength(z)) < (4))
        {
            z = (squareComplex(z)) + (p);
            steps = (steps) + (1);
        }

        i = (i) + (1);
    }

    if ((steps) == (maxSteps))
    {
        return float4(1, 0, 0, 1);
    }
    else
    {
        return float4((steps) / (15), 0, 0, (steps) / (15));
    }

}


            float4 frag (v2f i) : SV_Target
            {
                __globaluv = i.uv;
                float4 col = __cast(main());
                col.rgb = LinearToGammaSpace(col.rgb);
                return col;
            }
            ENDCG
        }
    }
}
