// The machines' fish, and the signal-flow chart's other creatures (the kraken
// for purerl-tidal, the lanternfish for link-spike, the grouper for Itajara)
// and its headphones: one SVG sprite of <symbol>s, added to the page once, so
// any page can draw a machine's fish with <use href="#sp-<slot>">. Drawn in the
// identity study (docs/kb/plans/assets/reef-specimens.html).
const SPRITE = `<svg id="tf-fish" width="0" height="0" style="position:absolute" aria-hidden="true">
  <defs>
    <path id="p-body" d="M14,63 C20,50 40,30 72,26 C100,22 124,32 142,46 L158,56 L158,70 L142,80 C124,94 100,104 72,100 C44,96 22,78 14,66 Z"/>
    <path id="p-dorsal" d="M100,27 C116,12 138,18 150,43 L141,46 C130,36 116,31 100,31 Z"/>
    <path id="p-anal" d="M100,99 C116,112 138,106 150,83 L141,80 C130,90 116,95 100,95 Z"/>
    <path id="p-spine" d="M63,29 L69,4 L76,27 Z M79,26 L82,15 L85,25 Z"/>
    <path id="p-pectoral" d="M72,62 C82,54 92,58 90,67 C84,70 77,69 72,62 Z"/>
    <path id="t-crescent" d="M156,57 C168,46 180,36 192,30 C182,50 182,76 192,96 C180,90 168,80 156,69 Z"/>
    <path id="t-lyre" d="M156,57 C170,44 184,24 198,10 C176,44 176,82 198,116 C184,102 170,82 156,69 Z"/>
    <path id="t-round" d="M156,57 L180,46 C188,56 188,70 180,80 L156,69 Z"/>
    <clipPath id="c-body"><use href="#p-body"/></clipPath>
    <clipPath id="c-moon"><path d="M24,74 L52,16 C92,10 132,32 152,56 L164,58 L164,68 L152,70 C132,94 92,110 52,106 Z"/></clipPath>

    <linearGradient id="g-odonus" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0" stop-color="#23306f"/><stop offset=".6" stop-color="#2447a0"/><stop offset="1" stop-color="#1c2f78"/>
    </linearGradient>
    <linearGradient id="g-vetula" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0" stop-color="#a88a2c"/><stop offset=".45" stop-color="#b99a3a"/><stop offset=".62" stop-color="#7f8d86"/><stop offset="1" stop-color="#5d7b98"/>
    </linearGradient>
    <linearGradient id="g-balistes" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0" stop-color="#7d846a"/><stop offset="1" stop-color="#a3a88f"/>
    </linearGradient>
    <linearGradient id="g-moon" x1="0" y1="0" x2="1" y2="1">
      <stop offset="0" stop-color="#eef1f4"/><stop offset=".5" stop-color="#c3cad3"/><stop offset=".78" stop-color="#a99bc6"/><stop offset="1" stop-color="#8d98a8"/>
    </linearGradient>
  </defs>

  <!-- Odonus niger: blue-black, pale-edged fins, red teeth, the lyre tail. -->
  <symbol id="sp-odonus" viewBox="0 0 200 120">
    <use href="#t-lyre" fill="#1d2c6c" stroke="#8fb4f0" stroke-width="2" style="display:var(--fine,inline)"/>
    <use href="#t-lyre" fill="#1d2c6c"/>
    <use href="#p-dorsal" fill="#26378a"/>
    <use href="#p-anal" fill="#26378a"/>
    <g style="display:var(--fine,inline)" fill="none" stroke="#8fb4f0" stroke-width="1.6">
      <path d="M104,28 C118,16 136,20 148,42"/><path d="M104,98 C118,110 136,106 148,84"/>
    </g>
    <use href="#p-spine" fill="#1d2c6c"/>
    <use href="#p-body" fill="url(#g-odonus)"/>
    <g clip-path="url(#c-body)" style="display:var(--detail,inline)">
      <path d="M40,30 C60,50 64,80 46,100" fill="none" stroke="#3b5cc0" stroke-width="7" opacity=".35"/>
    </g>
    <use href="#p-pectoral" fill="#2d3f94"/>
    <path d="M65,54 C61,60 61,68 65,74" fill="none" stroke="#12194a" stroke-width="1.5" style="display:var(--detail,inline)"/>
    <path d="M13,60 L20,62 L14,64 Z M14,65 L20,66 L15,69 Z" fill="#d6352b"/>
    <circle cx="56" cy="45" r="6" fill="#0c1233"/><circle cx="56" cy="45" r="2.6" fill="#8fb4f0"/>
  </symbol>

  <!-- Balistes vetula: gold over blue-grey, two blue face lines, dark lines round the eye. -->
  <symbol id="sp-vetula" viewBox="0 0 200 120">
    <use href="#t-lyre" fill="#8a7a3c"/>
    <path d="M196,12 C200,6 202,2 204,-2 M196,114 C200,120 202,124 204,128" stroke="#2fa3d6" stroke-width="1.6" fill="none" style="display:var(--fine,inline)"/>
    <use href="#p-dorsal" fill="#9a8a3e"/>
    <use href="#p-anal" fill="#6f8aa0"/>
    <g style="display:var(--fine,inline)" fill="none" stroke="#2fa3d6" stroke-width="1.4">
      <path d="M104,28 C118,16 136,20 148,42"/><path d="M104,98 C118,110 136,106 148,84"/>
      <path d="M160,58 C172,50 184,34 196,14"/><path d="M160,68 C172,76 184,92 196,112"/>
    </g>
    <use href="#p-spine" fill="#7c6c2c"/>
    <use href="#p-body" fill="url(#g-vetula)"/>
    <g clip-path="url(#c-body)" fill="none" stroke-linecap="round">
      <path d="M14,70 C38,78 60,80 86,74" stroke="#2fa3d6" stroke-width="4"/>
      <path d="M18,78 C40,90 66,92 94,84" stroke="#2fa3d6" stroke-width="3.2" style="display:var(--detail,inline)"/>
      <g stroke="#3b3320" stroke-width="1.4" style="display:var(--detail,inline)">
        <path d="M49,36 L44,30"/><path d="M56,36 L56,29"/><path d="M63,37 L68,31"/><path d="M47,51 L41,55"/><path d="M65,51 L71,55"/>
      </g>
    </g>
    <use href="#p-pectoral" fill="#9aa8a6"/>
    <path d="M65,54 C61,60 61,68 65,74" fill="none" stroke="#4a4526" stroke-width="1.5" style="display:var(--detail,inline)"/>
    <circle cx="56" cy="45" r="6" fill="#2d2716"/><circle cx="56" cy="45" r="2.6" fill="#d8c27a"/>
  </symbol>

  <!-- Balistes capriscus: olive-grey with darker saddles and blue dots. -->
  <symbol id="sp-balistes" viewBox="0 0 200 120">
    <use href="#t-crescent" fill="#6e755e"/>
    <use href="#p-dorsal" fill="#737a62"/>
    <use href="#p-anal" fill="#858b72"/>
    <use href="#p-spine" fill="#5d6450"/>
    <use href="#p-body" fill="url(#g-balistes)"/>
    <g clip-path="url(#c-body)">
      <path d="M78,20 L96,20 L90,58 L74,58 Z" fill="#5c634d" opacity=".55"/>
      <path d="M106,20 L124,26 L114,60 L100,58 Z" fill="#5c634d" opacity=".45" style="display:var(--detail,inline)"/>
      <path d="M132,30 L146,42 L134,62 L122,60 Z" fill="#5c634d" opacity=".4" style="display:var(--detail,inline)"/>
      <g fill="#7fa6c4" style="display:var(--fine,inline)">
        <circle cx="84" cy="38" r="1.6"/><circle cx="96" cy="44" r="1.6"/><circle cx="110" cy="36" r="1.6"/><circle cx="122" cy="46" r="1.6"/><circle cx="104" cy="52" r="1.6"/><circle cx="90" cy="54" r="1.6"/><circle cx="134" cy="52" r="1.6"/>
      </g>
    </g>
    <use href="#p-pectoral" fill="#6e755e"/>
    <path d="M65,54 C61,60 61,68 65,74" fill="none" stroke="#4d5340" stroke-width="1.5" style="display:var(--detail,inline)"/>
    <circle cx="56" cy="45" r="6" fill="#2a2e22"/><circle cx="56" cy="45" r="2.6" fill="#c7cdb1"/>
  </symbol>

  <!-- Balistoides conspicillum, reduced: black, three white spots, a yellow mouth. -->
  <symbol id="sp-conspicillum" viewBox="0 0 200 120">
    <use href="#t-round" fill="#17171b"/>
    <use href="#p-dorsal" fill="#1d1d22"/>
    <use href="#p-anal" fill="#1d1d22"/>
    <use href="#p-spine" fill="#17171b"/>
    <use href="#p-body" fill="#17171b"/>
    <g clip-path="url(#c-body)" fill="#f2f2ee">
      <circle cx="74" cy="94" r="11"/><circle cx="102" cy="97" r="12"/><circle cx="130" cy="86" r="11"/>
    </g>
    <path d="M10,59 C15,56 21,59 22,64 C21,69 15,72 10,69 Z" fill="#e6c02e"/>
    <circle cx="56" cy="45" r="6" fill="#0a0a0c"/><circle cx="56" cy="45" r="2.6" fill="#e6c02e"/>
  </symbol>

  <!-- Limulus: a horseshoe crab, from above. Not a fish, and not a machine:
       the live-coding editor, drawn among them because it plays them. -->
  <symbol id="sp-limulus" viewBox="0 0 200 120">
    <path d="M132,57 L197,59 L197,61 L132,63 Z" fill="#4a4127"/>
    <path d="M90,28 L134,42 L134,78 L90,92 Z" fill="#6a5b33"/>
    <path d="M98,30 L102,24 L106,33 M112,35 L116,29 L120,38 M98,90 L102,96 L106,87 M112,85 L116,91 L120,82" stroke="#4a4127" stroke-width="3" fill="none"/>
    <path d="M94,16 C46,12 12,34 12,60 C12,86 46,108 94,104 C88,96 86,88 92,80 L92,40 C86,32 88,24 94,16 Z" fill="#86733f"/>
    <path d="M24,60 C40,52 66,50 90,54 M24,60 C40,68 66,70 90,66" stroke="#a8925a" stroke-width="3" fill="none" style="display:var(--detail,inline)"/>
    <ellipse cx="54" cy="36" rx="5" ry="3" fill="#2b2516"/><ellipse cx="54" cy="84" rx="5" ry="3" fill="#2b2516"/>
  </symbol>

  <!-- Quadrat: a boxfish (Ostracion). A near-square body whose shell is a grid of plates. -->
  <symbol id="sp-quadrat" viewBox="0 0 200 120">
    <path d="M148,58 C162,42 180,40 188,46 C183,58 183,68 188,80 C180,86 162,84 148,68 Z" fill="#a8471f"/>
    <path d="M110,20 C116,6 130,6 136,20 Z M110,100 C116,114 130,114 136,100 Z" fill="#a8471f"/>
    <rect x="38" y="18" width="112" height="84" rx="18" fill="#c4592b"/>
    <clipPath id="c-box"><rect x="38" y="18" width="112" height="84" rx="18"/></clipPath>
    <g clip-path="url(#c-box)">
      <path d="M66,18 V102 M94,18 V102 M122,18 V102 M38,46 H150 M38,74 H150" stroke="#f3d3bd" stroke-width="3" style="display:var(--detail,inline)"/>
      <g fill="#f3d3bd" style="display:var(--fine,inline)">
        <circle cx="80" cy="32" r="3"/><circle cx="108" cy="32" r="3"/><circle cx="136" cy="32" r="3"/>
        <circle cx="80" cy="60" r="3"/><circle cx="108" cy="60" r="3"/><circle cx="136" cy="60" r="3"/>
        <circle cx="52" cy="88" r="3"/><circle cx="80" cy="88" r="3"/><circle cx="108" cy="88" r="3"/><circle cx="136" cy="88" r="3"/>
      </g>
    </g>
    <path d="M84,64 C92,58 100,62 98,70 C92,72 87,70 84,64 Z" fill="#8f3a17"/>
    <ellipse cx="33" cy="70" rx="7" ry="5.5" fill="#8f3a17"/>
    <circle cx="58" cy="40" r="8" fill="#2a1409"/><circle cx="58" cy="40" r="3.4" fill="#f3d3bd"/>
  </symbol>

  <!-- Sufflamen bridled: brown, a pale bridle line from the mouth, retired. -->
  <symbol id="sp-sufflamen" viewBox="0 0 200 120">
    <use href="#t-round" fill="#6a4c33"/>
    <path d="M180,46 C188,56 188,70 180,80" fill="none" stroke="#efe2c6" stroke-width="2" style="display:var(--detail,inline)"/>
    <use href="#p-dorsal" fill="#6f5036"/>
    <use href="#p-anal" fill="#6f5036"/>
    <use href="#p-spine" fill="#5c412b"/>
    <use href="#p-body" fill="#7b5b3e"/>
    <g clip-path="url(#c-body)">
      <path d="M16,67 C34,70 50,70 68,62" fill="none" stroke="#efe2c6" stroke-width="3.2" stroke-linecap="round"/>
      <path d="M40,94 C70,100 110,96 140,82" fill="none" stroke="#5c412b" stroke-width="8" opacity=".5" style="display:var(--detail,inline)"/>
    </g>
    <use href="#p-pectoral" fill="#6a4c33"/>
    <circle cx="56" cy="45" r="6" fill="#2a1d12"/><circle cx="56" cy="45" r="2.6" fill="#e0c79a"/>
  </symbol>

  <!-- Selene vomer, the lookdown: another family. Steep forehead, silver, long fin filaments. -->
  <symbol id="sp-selene" viewBox="0 0 200 120">
    <path d="M160,58 L192,34 L178,63 L192,92 L160,68 Z" fill="#aab3bf"/>
    <g fill="none" stroke="#8d7fb4" stroke-width="1.6" stroke-linecap="round" style="display:var(--fine,inline)">
      <path d="M86,14 C96,-2 122,-6 150,2"/><path d="M88,110 C100,122 126,124 150,118"/>
    </g>
    <path d="M24,74 L52,16 C92,10 132,32 152,56 L164,58 L164,68 L152,70 C132,94 92,110 52,106 Z" fill="url(#g-moon)"/>
    <g clip-path="url(#c-moon)" style="display:var(--detail,inline)">
      <path d="M40,40 C80,60 120,60 160,50" fill="none" stroke="#ffffff" stroke-width="10" opacity=".45"/>
    </g>
    <path d="M80,72 C88,66 96,68 96,74 C90,78 84,78 80,72 Z" fill="#b8b0d4"/>
    <circle cx="50" cy="70" r="5.5" fill="#3a3550"/><circle cx="50" cy="70" r="2.3" fill="#e6e2f4"/>
  </symbol>
  <symbol id="ic-kraken" viewBox="12 16 77 77">
  <path d="M46.2,54.1C44.8,51.6 42.8,49.3 41.3,46.8C39.7,44.2 37.6,42 35.5,39.5C33.5,37.1 30.6,35.3 27.5,33.9C24.4,32.6 20.6,33 17.6,34.8C14.6,36.7 13.8,41.3 15.9,44A0.8,0.8 0 0 1 17.1,43.1C15.6,41.2 17,38.4 19,37.1C21,35.9 23.7,36.5 26,37.5C28.3,38.5 29.9,40.7 31.6,42.8C33.4,44.9 34.4,47.6 35.9,50C37.3,52.5 38.3,55.3 39.8,57.9ZM60.2,57.9C61.7,55.3 62.7,52.5 64.1,50C65.6,47.6 66.6,44.9 68.4,42.8C70.1,40.7 71.7,38.5 74,37.5C76.3,36.5 79,35.9 81,37.1C83,38.4 84.4,41.2 82.9,43.1A0.8,0.8 0 0 1 84.1,44C86.2,41.3 85.4,36.7 82.4,34.8C79.4,33 75.6,32.6 72.5,33.9C69.4,35.3 66.5,37.1 64.5,39.5C62.4,42 60.3,44.2 58.7,46.8C57.2,49.3 55.2,51.6 53.8,54.1ZM44,56.4C41.3,55.7 38.6,55.6 35.8,55.1C33.1,54.5 30.3,54.7 27.3,54.6C24.4,54.6 21.5,55.5 18.7,56.7C15.9,58 13.8,60.7 12.8,63.7C11.8,66.7 13.6,70.3 16.4,71.6A0.8,0.8 0 0 1 17.1,70.3C15.1,69.3 14.7,66.6 15.4,64.5C16,62.5 18.2,61.2 20.2,60.3C22.3,59.4 24.8,59.7 27.2,59.7C29.7,59.8 32.1,60.7 34.6,61.2C37.1,61.7 39.5,62.9 42,63.6ZM58,63.6C60.5,62.9 62.9,61.7 65.4,61.2C67.9,60.7 70.3,59.8 72.8,59.7C75.2,59.7 77.7,59.4 79.8,60.3C81.8,61.2 84,62.5 84.6,64.5C85.3,66.6 84.9,69.3 82.9,70.3A0.8,0.8 0 0 1 83.6,71.6C86.4,70.3 88.2,66.7 87.2,63.7C86.2,60.7 84.1,58 81.3,56.7C78.5,55.5 75.6,54.6 72.7,54.6C69.7,54.7 66.9,54.5 64.2,55.1C61.4,55.6 58.7,55.7 56,56.4Z" style="fill:var(--kraken-deep,#5e2234)"/>
  <g style="fill:var(--kraken,#8a3a50)"><path d="M40.8,58.8C38.9,60.7 37.6,63.1 35.7,64.9C33.8,66.8 32.5,69.1 30.5,70.6C28.6,72.1 26.9,73.9 24.7,74.6C22.5,75.3 20.1,76 18.2,75.1C16.2,74.2 14.1,72.5 14.4,70.2A0.8,0.8 0 0 1 12.9,70C12.5,73.1 14,76.6 16.9,77.9C19.8,79.2 23.1,79.8 26,78.9C29,78 31.9,77.1 34.2,75.3C36.5,73.6 39,72.2 40.9,70.3C42.9,68.4 45.3,67.1 47.2,65.2ZM52.8,65.2C54.7,67.1 57.1,68.4 59.1,70.3C61,72.2 63.5,73.6 65.8,75.3C68.1,77.1 71,78 74,78.9C76.9,79.8 80.2,79.2 83.1,77.9C86,76.6 87.5,73.1 87.1,70A0.8,0.8 0 0 1 85.6,70.2C85.9,72.5 83.8,74.2 81.8,75.1C79.9,76 77.5,75.3 75.3,74.6C73.1,73.9 71.4,72.1 69.5,70.6C67.5,69.1 66.2,66.8 64.3,64.9C62.4,63.1 61.1,60.7 59.2,58.8ZM42.8,62.6C42.6,64.7 43,66.9 42.8,68.9C42.5,71 42.9,73.1 42.4,74.9C41.9,76.7 41.8,78.7 40.7,80.1C39.7,81.5 38.6,83.1 36.9,83.4C35.3,83.6 33.2,83.5 32.4,82A0.8,0.8 0 0 1 31.1,82.7C32.3,84.9 34.9,86.7 37.4,86.2C40,85.8 42.6,84.7 44.2,82.7C45.7,80.7 47.3,78.7 47.9,76.4C48.5,74.1 49.5,72 49.8,69.8C50.1,67.6 51,65.6 51.2,63.4ZM48.8,63.4C49,65.6 49.9,67.6 50.2,69.8C50.5,72 51.5,74.1 52.1,76.4C52.7,78.7 54.3,80.7 55.8,82.7C57.4,84.7 60,85.8 62.6,86.2C65.1,86.7 67.7,84.9 68.9,82.7A0.8,0.8 0 0 1 67.6,82C66.8,83.5 64.7,83.6 63.1,83.4C61.4,83.1 60.3,81.5 59.3,80.1C58.2,78.7 58.1,76.7 57.6,74.9C57.1,73.1 57.5,71 57.2,68.9C57,66.9 57.4,64.7 57.2,62.6Z"/><path d="M50,22 C61,22 66,34 64,46 C63,54 60,58 60,62 C60,68 40,68 40,62 C40,58 37,54 36,46 C34,34 39,22 50,22 Z"/></g>
  <circle cx="42.6" cy="56" r="3" fill="#f3d36b"/><rect x="40.6" y="55.3" width="4" height="1.4" rx=".7" style="fill:var(--kraken-pupil,#2a0f18)"/>
  <circle cx="57.4" cy="56" r="3" fill="#f3d36b"/><rect x="55.4" y="55.3" width="4" height="1.4" rx=".7" style="fill:var(--kraken-pupil,#2a0f18)"/>
</symbol>
  <symbol id="ic-lantern" viewBox="0 0 60 36">
    <path d="M6,18 C14,6 38,4 48,14 L58,6 L56,18 L58,30 L48,22 C38,32 14,30 6,18 Z" fill="#9fb0b8"/>
    <circle cx="16" cy="16" r="3.4" fill="#f6d86b"/><circle cx="28" cy="22" r="1.6" fill="#f6d86b"/><circle cx="36" cy="22" r="1.6" fill="#f6d86b"/>
  </symbol>
  <symbol id="ic-grouper" viewBox="0 0 70 40">
    <path d="M4,20 C14,4 46,2 56,14 L68,6 L66,20 L68,34 L56,26 C46,38 14,36 4,20 Z" fill="#6a5a3c"/>
    <circle cx="14" cy="17" r="2.6" fill="#1c160c"/>
  </symbol>
  <symbol id="ic-ears" viewBox="0 0 48 48">
    <path d="M8,30 C8,14 16,6 24,6 C32,6 40,14 40,30" fill="none" style="stroke:var(--ink,#0f2530)" stroke-width="3.4"/>
    <rect x="4" y="27" width="10" height="16" rx="3" style="fill:var(--ink,#0f2530)"/><rect x="34" y="27" width="10" height="16" rx="3" style="fill:var(--ink,#0f2530)"/>
  </symbol>
</svg>`;

export const install = () => {
  if (document.getElementById("tf-fish")) return;
  const holder = document.createElement("div");
  holder.innerHTML = SPRITE;
  document.body.prepend(holder.firstElementChild);
};
