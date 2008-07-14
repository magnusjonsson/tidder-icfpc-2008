#include <allegro.h>
#include <iostream>
#include <string>
#include <algorithm>
#include <cassert>

using namespace std;

BITMAP* offscreen;

double dx = 100.0;
double dy = 100.0;

int sx(double x) { return 512*(0.5+0.5*x/max(dx,dy)); }
int sy(double y) { return 512*(0.5-0.5*y/max(dx,dy)); }
int sr(double r) { return 512*0.5*r/max(dx,dy); }

int color = 0;

int main() {
  allegro_init();
  set_gfx_mode(GFX_AUTODETECT_WINDOWED,512,512,0,0);
  offscreen = create_bitmap(512,512);
  assert(offscreen);

  while(true) {
    string cmd;
    cin >> cmd;
    if (!cin)
      break;
    if (cmd == "quit") { break; }
    else if (cmd == "dx") { cin >> dx; }
    else if (cmd == "dy") { cin >> dy; }
    else if (cmd == "clear") {
      clear_to_color(offscreen,color);
    }
    else if (cmd == "show") {
      blit(offscreen,screen,0,0,0,0,offscreen->w,offscreen->h);
    }
    else if (cmd == "color") {
      int r=0,g=0,b=0;
      cin>>r>>g>>b;
      color = makecol(r,g,b);
    }
    else if (cmd == "line") {
      double x0=0,y0=0,x1=0,y1=0;
      cin>>x0>>y0>>x1>>y1;
      line(offscreen,sx(x0),sy(y0),sx(x1),sy(y1),color); }
    else if (cmd == "circle") {
      double x=0,y=0,r=0;
      cin>>x>>y>>r;
      circle(offscreen,sx(x),sy(y),sr(r),color);
    }
  }
  return 0;
}
END_OF_MAIN()
