using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class BoltScript : MonoBehaviour
{
    float startX;
    float startY;
    // Start is called before the first frame update
    void Start()
    {
        startX = gameObject.transform.position.x;
        startY = gameObject.transform.position.y;
    }

    // Update is called once per frame
    void Update()
    {
        transform.Translate(0, -2.5f * Time.deltaTime, 0);
    }

    private void OnTriggerEnter2D(Collider2D collision)
    {
        if (collision.gameObject.CompareTag("BoltReset"))
        {
            //gameObject.transform.position = new Vector3(-1.8f, 1.3f, 0);
            gameObject.transform.position = new Vector3(startX, startY, 0);
            //Instantiate(this);
            //gameObject.SetActive(false);
        }
    }

    public void Deactivate()
    {
        gameObject.SetActive(false);
    }
}
