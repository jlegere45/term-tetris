use crossterm::cursor::{Hide, MoveTo, Show};
use crossterm::event::{poll, read, Event, KeyCode, KeyEvent, KeyModifiers};
use crossterm::execute;
use crossterm::style::{Color, Print, ResetColor, SetBackgroundColor, SetForegroundColor};
use crossterm::terminal::{disable_raw_mode, enable_raw_mode, Clear, ClearType, EnterAlternateScreen, LeaveAlternateScreen};
use rand::rng;
use rand::seq::IndexedRandom; // rand 0.9 trait for choose()
use std::cmp::min;
use std::io::{self, stdout, Write};
use std::time::{Duration, Instant};

// Unify error type regardless of crossterm alias availability

const W: usize = 10; // board width
const H: usize = 20; // board height

#[derive(Copy, Clone, Debug)]
enum Kind { I, O, T, S, Z, J, L }

#[derive(Copy, Clone)]
struct Cell { kind: Kind }

#[derive(Clone)]
struct Board {
    grid: [[Option<Cell>; W]; H],
}

impl Board {
    fn new() -> Self { Self { grid: [[None; W]; H] } }
    fn inside(x: i32, y: i32) -> bool { x >= 0 && x < W as i32 && y >= 0 && y < H as i32 }
    fn get(&self, x: i32, y: i32) -> Option<Cell> {
        if Self::inside(x, y) { self.grid[y as usize][x as usize] } else { None }
    }
    fn set(&mut self, x: i32, y: i32, c: Option<Cell>) {
        if Self::inside(x, y) { self.grid[y as usize][x as usize] = c; }
    }
    fn lock_piece(&mut self, p: &Piece) {
        for (dx, dy) in p.blocks() { self.set(p.x + dx, p.y + dy, Some(Cell { kind: p.kind })); }
    }
    fn clear_lines(&mut self) -> usize {
        let mut write_y = H as i32 - 1;
        let mut cleared = 0;
        for y in (0..H as i32).rev() {
            let full = (0..W as i32).all(|x| self.get(x, y).is_some());
            if !full {
                if write_y != y { self.grid[write_y as usize] = self.grid[y as usize]; }
                write_y -= 1;
            } else { cleared += 1; }
        }
        for y in 0..=write_y { self.grid[y as usize] = [None; W]; }
        cleared
    }
    fn collides(&self, p: &Piece) -> bool {
        for (dx, dy) in p.blocks() {
            let x = p.x + dx; let y = p.y + dy;
            if !Self::inside(x, y) { return true; }
            if self.get(x, y).is_some() { return true; }
        }
        false
    }
}

#[derive(Copy, Clone)]
struct Piece { kind: Kind, rot: u8, x: i32, y: i32 }

impl Piece {
    fn new(kind: Kind) -> Self { Self { kind, rot: 0, x: 3, y: 0 } }
    fn shape(kind: Kind) -> &'static [[(i32, i32); 4]; 4] {
        match kind {
            Kind::I => &[
                [(0,1),(1,1),(2,1),(3,1)],
                [(2,0),(2,1),(2,2),(2,3)],
                [(0,2),(1,2),(2,2),(3,2)],
                [(1,0),(1,1),(1,2),(1,3)],
            ],
            Kind::O => &[
                [(1,0),(2,0),(1,1),(2,1)],
                [(1,0),(2,0),(1,1),(2,1)],
                [(1,0),(2,0),(1,1),(2,1)],
                [(1,0),(2,0),(1,1),(2,1)],
            ],
            Kind::T => &[
                [(1,0),(0,1),(1,1),(2,1)],
                [(1,0),(1,1),(2,1),(1,2)],
                [(0,1),(1,1),(2,1),(1,2)],
                [(1,0),(0,1),(1,1),(1,2)],
            ],
            Kind::S => &[
                [(1,0),(2,0),(0,1),(1,1)],
                [(1,0),(1,1),(2,1),(2,2)],
                [(1,1),(2,1),(0,2),(1,2)],
                [(0,0),(0,1),(1,1),(1,2)],
            ],
            Kind::Z => &[
                [(0,0),(1,0),(1,1),(2,1)],
                [(2,0),(1,1),(2,1),(1,2)],
                [(0,1),(1,1),(1,2),(2,2)],
                [(1,0),(0,1),(1,1),(0,2)],
            ],
            Kind::J => &[
                [(0,0),(0,1),(1,1),(2,1)],
                [(1,0),(2,0),(1,1),(1,2)],
                [(0,1),(1,1),(2,1),(2,2)],
                [(1,0),(1,1),(0,2),(1,2)],
            ],
            Kind::L => &[
                [(2,0),(0,1),(1,1),(2,1)],
                [(1,0),(1,1),(1,2),(2,2)],
                [(0,1),(1,1),(2,1),(0,2)],
                [(0,0),(1,0),(1,1),(1,2)],
            ],
        }
    }
    fn blocks(&self) -> [(i32, i32); 4] { Self::shape(self.kind)[self.rot as usize] }
    fn try_move(&mut self, board: &Board, dx: i32, dy: i32) -> bool {
        let mut p = *self; p.x += dx; p.y += dy;
        if board.collides(&p) { false } else { *self = p; true }
    }
    fn try_rotate(&mut self, board: &Board, cw: bool) -> bool {
        let mut p = *self; p.rot = (p.rot + if cw {1} else {3}) % 4;
        for kick in [0, -1, 1, -2, 2] {
            let mut t = p; t.x += kick;
            if !board.collides(&t) { *self = t; return true; }
        }
        false
    }
}

fn color_for(kind: Kind) -> Color {
    match kind {
        Kind::I => Color::Cyan,
        Kind::O => Color::Yellow,
        Kind::T => Color::Magenta,
        Kind::S => Color::Green,
        Kind::Z => Color::Red,
        Kind::J => Color::Blue,
        Kind::L => Color::DarkYellow,
    }
}

struct Game {
    board: Board,
    cur: Piece,
    next: Kind,
    score: u64,
    level: u32,
    lines: u32,
    over: bool,
}

impl Game {
    fn new() -> Self {
        let mut rng = rng();
        let kinds = [Kind::I, Kind::O, Kind::T, Kind::S, Kind::Z, Kind::J, Kind::L];
        let first = *kinds.choose(&mut rng).unwrap();
        let next = *kinds.choose(&mut rng).unwrap();
        Self { board: Board::new(), cur: Piece::new(first), next, score: 0, level: 1, lines: 0, over: false }
    }
    fn spawn_next(&mut self) {
        let mut rng = rng();
        let kinds = [Kind::I, Kind::O, Kind::T, Kind::S, Kind::Z, Kind::J, Kind::L];
        self.cur = Piece::new(self.next);
        self.next = *kinds.choose(&mut rng).unwrap();
        if self.board.collides(&self.cur) { self.over = true; }
    }
}

fn gravity_delay_ms(level: u32) -> u64 {
    let base = 800u64; // ms at level 1
    let step = min(level.saturating_sub(1), 15) as u64 * 45;
    base.saturating_sub(step)
}

fn hard_drop(board: &Board, mut p: Piece) -> (Piece, i32) {
    let mut rows = 0;
    while !board.collides(&Piece { y: p.y + 1, ..p }) { p.y += 1; rows += 1; }
    (p, rows)
}

fn draw_ui<W: Write>(out: &mut W, g: &Game) -> io::Result<()> {
    execute!(out,
        MoveTo(0,0),
        SetForegroundColor(Color::White),
        Print("┌────────────┐    TERM TETRIS"),
        MoveTo(0,1), Print("│"), MoveTo((W*2 + 1) as u16, 1), Print("│"),
        MoveTo(0, (H+1) as u16), Print("└"), MoveTo((W*2 + 1) as u16, (H+1) as u16), Print("┘"),
        ResetColor
    )?;
    for y in 0..H {
        execute!(out, MoveTo(0, (y+1) as u16), Print("│"))?;
        execute!(out, MoveTo((W*2 + 1) as u16, (y+1) as u16), Print("│"))?;
    }
    execute!(out,
        MoveTo((W*2 + 4) as u16, 2), Print(format!("Score: {}", g.score)),
        MoveTo((W*2 + 4) as u16, 3), Print(format!("Level: {}", g.level)),
        MoveTo((W*2 + 4) as u16, 4), Print(format!("Lines: {}", g.lines)),
        MoveTo((W*2 + 4) as u16, 6), Print("Next:"),
    )?;
    let preview = Piece { kind: g.next, rot: 0, x: (W as i32 + 6), y: 8 };
    draw_piece(out, &preview, true)?;
    Ok(())
}

fn draw_board<W: Write>(out: &mut W, b: &Board) -> io::Result<()> {
    for y in 0..H as i32 {
        for x in 0..W as i32 {
            if let Some(cell) = b.get(x, y) {
                draw_block(out, 1 + x*2, 1 + y, color_for(cell.kind))?;
            } else {
                execute!(out, MoveTo((1 + x*2) as u16, (1 + y) as u16), Print("  "))?;
            }
        }
    }
    Ok(())
}

fn draw_piece<W: Write>(out: &mut W, p: &Piece, _preview: bool) -> io::Result<()> {
    let col = color_for(p.kind);
    for (dx, dy) in p.blocks() {
        let x = p.x + dx; let y = p.y + dy;
        if !Board::inside(x, y) { continue; }
        draw_block(out, 1 + x*2, 1 + y, col)?;
    }
    Ok(())
}

fn draw_block<W: Write>(
    out: &mut W,
    x: i32,
    y: i32,
    color: crossterm::style::Color,
) -> std::io::Result<()> {
    use crossterm::{cursor::MoveTo, execute, style::{Print, ResetColor, SetBackgroundColor}};
    execute!(
        out,
        MoveTo(x as u16, y as u16),
        SetBackgroundColor(color),
        Print("  "),
        ResetColor
    )
}

fn main() -> io::Result<()> {
    use crossterm::{
        cursor::MoveTo,
        execute,
        style::{Print, ResetColor, SetForegroundColor, Color},
        terminal::{disable_raw_mode, enable_raw_mode, Clear, ClearType, EnterAlternateScreen, LeaveAlternateScreen},
    };

    let mut stdout = stdout();
    enable_raw_mode()?;
    execute!(stdout, EnterAlternateScreen, Hide)?;

    let mut game = Game::new();
    let mut last_grav = Instant::now();
    let mut gravity = Duration::from_millis(gravity_delay_ms(game.level));

    // Initial full draw (once)
    execute!(stdout, Clear(ClearType::All))?;
    draw_ui(&mut stdout, &game)?;
    draw_board(&mut stdout, &game.board)?;
    draw_piece(&mut stdout, &game.cur, false)?;  
   stdout.flush()?;

    // Track previously drawn piece so we can erase only those cells
    #[derive(Copy, Clone)]
    struct DrawnPiece { kind: Kind, rot: u8, x: i32, y: i32 }
    let mut last_drawn: Option<DrawnPiece> =
        Some(DrawnPiece { kind: game.cur.kind, rot: game.cur.rot, x: game.cur.x, y: game.cur.y });

    // Flag for when the board has changed (lock/line clear)
    let mut board_dirty = false;

    'outer: loop {
        let now = Instant::now();

        // ---- Input ----
        if poll(Duration::from_millis(16))? {
            if let Event::Key(KeyEvent { code, modifiers, .. }) = read()? {
                match code {
                    KeyCode::Char('q') | KeyCode::Esc => break 'outer,
                    KeyCode::Left  | KeyCode::Char('a') => { let mut p = game.cur; if p.try_move(&game.board, -1, 0) { game.cur = p; } }
                    KeyCode::Right | KeyCode::Char('d') => { let mut p = game.cur; if p.try_move(&game.board,  1, 0) { game.cur = p; } }
                    KeyCode::Down  | KeyCode::Char('s') => { let mut p = game.cur; if p.try_move(&game.board,  0, 1) { game.cur = p; game.score += 1; } }
                    KeyCode::Up | KeyCode::Char('w') | KeyCode::Char('k') => { let mut p = game.cur; if p.try_rotate(&game.board, true)  { game.cur = p; } }
                    KeyCode::Char('j')                 => { let mut p = game.cur; if p.try_rotate(&game.board, false) { game.cur = p; } }
                    KeyCode::Char(' ') => {
                        let (p, rows) = hard_drop(&game.board, game.cur);
                        game.cur = p; game.score += (rows as u64) * 2;
                    }
                    KeyCode::Char('c') if modifiers.contains(KeyModifiers::CONTROL) => break 'outer,
                    _ => {}
                }
            }
        }

        // ---- Gravity tick ----
        if now.duration_since(last_grav) >= gravity {
            let mut p = game.cur;
            if !p.try_move(&game.board, 0, 1) {
                // Lock piece & possibly clear lines
                game.board.lock_piece(&p);
                let cleared = game.board.clear_lines();
                if cleared > 0 {
                    game.lines += cleared as u32;
                    game.score += match cleared { 1 => 100, 2 => 300, 3 => 500, 4 => 800, _ => 1000 } as u64 * (game.level as u64);
                    if game.lines / 10 >= game.level {
                        game.level += 1;
                        gravity = Duration::from_millis(gravity_delay_ms(game.level));
                    }
                }
                last_drawn = None;
                board_dirty = true;
                game.spawn_next();
                if game.over { break 'outer; }
            } else {
                game.cur = p;
            }
            last_grav = now;
        }

        // ---- Incremental redraw (no full Clear) ----

        // If board changed (locked/cleared), redraw board once
        if board_dirty {
            draw_board(&mut stdout, &game.board)?;
            board_dirty = false;
        }

        // Erase previously drawn piece cells
        if let Some(prev) = last_drawn {
            let tmp = Piece { kind: prev.kind, rot: prev.rot, x: prev.x, y: prev.y };
            for (dx, dy) in tmp.blocks() {
                let sx = 1 + (tmp.x + dx) * 2;
                let sy = 1 + (tmp.y + dy);
                execute!(stdout, MoveTo(sx as u16, sy as u16), ResetColor, Print("  "))?;
            }
        }

        // Draw current piece cells
        draw_piece(&mut stdout, &game.cur, false)?;

        // Optionally refresh HUD numbers (cheap single-line updates)
        execute!(
            stdout,
            MoveTo((W as u16 * 2 + 4), 2), Print(format!("Score: {}", game.score)),
            MoveTo((W as u16 * 2 + 4), 3), Print(format!("Level: {}", game.level)),
            MoveTo((W as u16 * 2 + 4), 4), Print(format!("Lines: {}", game.lines)),
        )?;

        last_drawn = Some(DrawnPiece { kind: game.cur.kind, rot: game.cur.rot, x: game.cur.x, y: game.cur.y });
        stdout.flush()?;
    }

    // Footer
    execute!(
        stdout,
        MoveTo(0, H as u16 + 3),
        SetForegroundColor(Color::White),
        Print("Game Over. Thanks for playing.\n"),
        ResetColor
    )?;

    disable_raw_mode()?;
    execute!(stdout, Show, LeaveAlternateScreen)?;
    Ok(())
}
