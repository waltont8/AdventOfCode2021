// Part 1 or 2
int part = 2;


final int W = 0;
final int A = 1;
final int B = 2;
final int C = 3;
final int D = 4;
final int S = 5;

int bWidth = 13;
int bHeight = 5;
final int mode2 = 2;
final int xOffset = 10;
final int yOffset = 10;
final int tSize = 25;

ArrayList<Integer> undo = new ArrayList<Integer>();

// ArrayList is ridiculous
void pushit(int a)
{
  undo.add(a);
}

int popit()
{
  return undo.remove(undo.size()-1);
}




int score = 0;

int burrow[][];

// Part 1 input
int burrow1[][] = {
  {W,W,W,W,W,W,W,W,W,W,W,W,W},
  {W,S,S,S,S,S,S,S,S,S,S,S,W},
  {W,W,W,D,W,D,W,B,W,A,W,W,W},
  {W,W,W,C,W,A,W,B,W,C,W,W,W},
  {W,W,W,W,W,W,W,W,W,W,W,W,W}
};

//part 2 input
int burrow2[][] = {
  {W,W,W,W,W,W,W,W,W,W,W,W,W},
  {W,S,S,S,S,S,S,S,S,S,S,S,W},
  {W,W,W,D,W,D,W,B,W,A,W,W,W},
  {W,W,W,D,W,C,W,B,W,A,W,W,W},
  {W,W,W,D,W,B,W,A,W,C,W,W,W},  
  {W,W,W,C,W,A,W,B,W,C,W,W,W},
  {W,W,W,W,W,W,W,W,W,W,W,W,W}
};
  
  
  
int selectX = 0;
int selectY = 0;

Boolean selected = false;

void setup()
{
  size(800,600);
  if (part == 2)
  {
    bHeight = bHeight + mode2;
    burrow = burrow2;
  }
  else
  {
    bHeight = bHeight;
    burrow = burrow1;
  }
}

void draw()
{
  
  background (128);
  for (int x = 0; x<bWidth; x++)
  {
    for (int y = 0; y<bHeight; y++)
    {
      colorForTile(burrow[y][x]);
      rect(xOffset + x*tSize, yOffset+y*tSize,tSize,tSize);
      textSize(tSize);
      if (burrow[y][x] == S)
      fill(5);
        else
      fill(255);
        
      text(textLookup(burrow[y][x]), xOffset + x*tSize+5, yOffset+y*tSize+tSize-4);
    }
  }
  if (selected)
  {
    stroke(255,0,255);
    strokeWeight(4);
    fill(0,0,0,0);
    rect(xOffset + selectX*tSize, yOffset+selectY*tSize,tSize,tSize);
    strokeWeight(1);
    stroke(0,0,0);
  }
  fill(0);
  
  textSize(32);
  text("Score:" + score, 40, 240);
  
}

void colorForTile(int t)
{
  switch (t)
  {
    case W:
      fill(50,50,50);
      break;
    case S:
      fill(230,230,230);
      break;
    case A:
      fill(200,10,10);
      break;
    case B:
      fill(10,200,10);
      break;
    case C:
      fill(10,10,200);
      break;
    case D:
      fill(10,100,200);
      break;
  }
}

void mouseClicked()
{

  
  int x = mouseX; //<>//
  int y = mouseY;
  
  x = x - xOffset;
  y = y - yOffset;
  
  int tx = x/tSize;
  int ty = y/tSize;
  
  if (mouseButton == RIGHT)
  {
    if (undo.size() == 0) return;
 
    print("undo?");
        ty = popit();
        tx = popit();
        selectY = popit();
        selectX = popit();
        score = popit();
        selected = false;
        int tmp = burrow[selectY][selectX];
        burrow[selectY][selectX] = burrow[ty][tx];
        burrow[ty][tx] = tmp;

        return;
  }
  
  if ((tx >= bWidth) || (ty >= bHeight))
  {
     selected = false;
  }
  else
  {
    if (selected)
    {
      if (burrow[ty][tx] == S && ((abs(selectX-tx) + abs(selectY-ty)) == 1))
      {
        int tmp = burrow[selectY][selectX];
        pushit(score);
        pushit(selectX);
        pushit(selectY);
        pushit(tx);
        pushit(ty);
        
        score = score + scoreLookup(tmp);
        burrow[selectY][selectX] = burrow[ty][tx];
        burrow[ty][tx] = tmp;
        selectX = tx;
        selectY = ty;
      }
      else
      {
        selected = false;
      }
    }
    else if (burrow[ty][tx] != S && burrow[ty][tx] != W)
    {
      selectX = tx;
      selectY = ty;
      selected = true;
    }
  }

 
}


String textLookup(int t)
{
  switch (t)
  {
    case W:
      return "";
    case S:
      return "";
    case A:
      return "A";
    case B:
      return "B";
    case C:
      return "C";
    case D:
      return "D";
  }
  return "X";
}

int scoreLookup(int t)
{
  switch (t)
  {
    case W:
      return -100000;
    case S:
      return -100000;
    case A:
      return (1);
    case B:
      return (10);
    case C:
      return (100);
    case D:
      return (1000);
  }
  return -100000;
}
