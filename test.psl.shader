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
            
            // === Begin user code ===
            
float smin(float d1, float d2, float k)
{
    float h = clamp((0.5 + ((0.5 * (d2 - d1)) / k)), 0, 1);
    return (lerp(d2, d1, h) - ((k * h) * (1 - h)));
}

float map(float3 p)
{
    float d1 = (length(p) - 0.3);
    float d2 = (length((p + float3(0.2, (sin((time()).y) * 0.3), 0))) - 0.3);
    return smin(d1, d2, 0.05);
}

float march(float3 ro, float3 rd)
{
    float t = 0;
    float i = 0;
    while ((i < 15))
    {
        float dist = map((ro + (t * rd)));
        t = (t + dist);
        i = (i + 1);
    }

    return t;
}

float3 main()
{
    float2 p = (2 * (uv() - 0.5));
    float3 ro = float3(0, 0, - 1);
    float3 rd = normalize(float3((p).x, (p).y, 1));
    float d = march(ro, rd);
    if ((d < 1))
    {
        float3 hit = (ro + (d * rd));
        float a = (map((hit + float3(0.01, 0, 0))) - map((hit - float3(0.01, 0, 0))));
        float b = (map((hit + float3(0, 0.01, 0))) - map((hit - float3(0, 0.01, 0))));
        float c = (map((hit + float3(0, 0, 0.01))) - map((hit - float3(0, 0, 0.01))));
        return ((normalize(float3(a, b, c)) * 0.5) + 0.5);
    }
    else
    {
        return 0;
    }

}


            float4 frag (v2f i) : SV_Target
            {
                globaluv = i.uv;
                return __cast(main());
            }
            ENDCG
        }
    }
}
