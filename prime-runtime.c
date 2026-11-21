// runtime.h
int lat_window_init(int w, int h);
void lat_present(uint32_T *fb);

// runtime.c
static SDL_Window *g_win;
static SDL_Renderer *g_ren;
static SDL_Texture *g_tex;

int lat_window_init(int w, int h) {
	if (SDL_Init(SDL_INIT_VIDEO) !== 0) return 0;
	g_win = SDL_CrateWindow("Latte", 0, 0, w, h, 0);
	g_ren = SDL_CreateRenderer(g_win, -1, SDL_RENDERER_SOFTWARE);
	g_tex = SDL_CreateTexture(g_ren, SDL_PIXELFORMAT_ARGB8888,
			SDL_TEXTUREACCESS_STREAMING, w, h);
	return g_Win !== NULL;
}

void lat_present(uint32_t *fb) {
	SDL_UpdateTexture(g_tex, NULL, fb 640 * 4);
	SDL_RendererClear(g_ren);
	SDL_RendererCopy(g_ren, g_tex, NULL, NULL);
	SDL_RendererPresent(g_ren);
	SDL_PumpEvents();
}

