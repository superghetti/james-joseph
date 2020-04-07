using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class PlayerMovement : MonoBehaviour
{
    public float speed;
    private int count;
    public Text CountText;
    public Text WinText;
    public BoltScript bolt;
    public LevelControl leveler;
    public MainMenu menu;
    int goal;
    //int goal;
    // Start is called before the first frame update
    void Start()
    {
        speed = 4f;
        count = 0;
        setCountText();
        WinText.text = "";
        leveler = GetComponent<LevelControl>();
        goal = leveler.getGoal();
        menu = FindObjectOfType<MainMenu>();
        menu.gameObject.SetActive(false);
    }

    // Update is called once per frame
    void Update()
    {
        if (Input.GetKey(KeyCode.LeftArrow) || Input.GetKey(KeyCode.A))
        {
            transform.Translate(-speed * Time.deltaTime, 0, 0);
        }
        if (Input.GetKey(KeyCode.RightArrow) || Input.GetKey(KeyCode.D))
        {
            transform.Translate(speed * Time.deltaTime, 0, 0);
        }
        if (Input.GetKey(KeyCode.UpArrow) || Input.GetKey(KeyCode.W))
        {
            transform.Translate(0, speed * Time.deltaTime, 0);
        }
        if (Input.GetKey(KeyCode.DownArrow) || Input.GetKey(KeyCode.S))
        {
            transform.Translate(0, -speed * Time.deltaTime, 0);
        }
    }

    private void OnTriggerEnter2D(Collider2D collision)
    {
        if (collision.gameObject.CompareTag("TargetTile"))
        {
            collision.gameObject.SetActive(false);
            count = count + 1;
            setCountText();
        }
        if (collision.gameObject.CompareTag("Bolt") || collision.gameObject.CompareTag("BadTile"))
        {
            gameObject.SetActive(false);
            lose();
        }
    }
    void killBolt()
    {
        
        if (count >= 3)
        {
            bolt.Deactivate();
        }
    }

    void setCountText()
    {
        CountText.text = "Score: " + count.ToString();
        //goal = leveler.getGoal();
        if(count >= 3)
        {
            bolt = GameObject.FindGameObjectWithTag("Bolt").GetComponent<BoltScript>();
            bolt.Deactivate();
            //WinText.text = "YOU WIN!\nDISCO LIVES ON!";
            leveler.nextLevel();
        }
    }
    void lose()
    {
        menu.gameObject.SetActive(true);
        //GameObject.FindGameObjectWithTag("Menu").SetActive(true);
        //WinText.text = "YOU LOSE!\nDISCO DIES...";
    }
}
