import java.nio.file.{ Files, Paths }
import java.security.{ SecureRandom, KeyPairGenerator, PrivateKey, PublicKey }
import java.util.UUID
import java.util.concurrent.{ TimeUnit, ConcurrentHashMap }
import java.util.concurrent.atomic.AtomicInteger
import javax.crypto.Cipher

import Constants.{ PostTypes, Privacy }
import akka.actor.ActorSelection.toScala
import akka.actor.{ Actor, ActorLogging, ActorSystem, Cancellable, Props, actorRef2Scala }
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import org.apache.commons.codec.binary.Base64
import org.apache.commons.lang3.StringEscapeUtils
import spray.client.pipelining.sendReceive
import spray.http.ContentType.apply
import spray.http.{ HttpResponse, _ }
import spray.httpx.RequestBuilding._
import spray.httpx.SprayJsonSupport
import spray.json.AdditionalFormats

import scala.concurrent.{ Await, Future }
import scala.concurrent.duration.FiniteDuration
import scala.util.Random

object FacebookSimulator {

  var namingPrefix = "akka://" + "FBClients" + "/user/"
  val actorConf =
    """
  akka {
    #log-config-on-start = on
    stdout-loglevel = "INFO"
    loglevel = "INFO"
  }
    """
  val system = ActorSystem("FBClients", ConfigFactory.parseString(actorConf))
  implicit val timeout = Timeout(1)

  val userPrefix = "user"
  val totalUsers = Constants.totalUsers
  val activeUsers = ((2 * totalUsers) / 3)
  val posts = ((3 * activeUsers) / 5)
  val newuser = if (totalUsers * 0.000005 < 1) 1 else totalUsers * 0.000005
  val scPostTime = ((60 * 60) / posts) + 0.1

  val scAlbumTime = 10 * scPostTime
  val scPhotoTime = 2 * scPostTime
  val scUpdateTime = 5

  val scNewUser = 60 / newuser
  val scFrndReq = 60
  val scView = 0.01
  val postUserPer = (25 * activeUsers) / 100
  val postPhotoPer = (40 * activeUsers) / 100
  val viewPer = totalUsers
  var ALGORITHM = "RSA"

  var keyGen = KeyPairGenerator.getInstance(ALGORITHM)
  keyGen.initialize(1024)

  def main(args: Array[String]) {
    bootSystem()
    startSchedulers()
  }

  sealed trait seal

  case class getPage()

  case class getProfile(userId: String)

  case class updateProfile(u: User)

  case class addDefaultAlbum()

  case class addDynamicAlbumAndPhoto()

  case class addDefaultImages()

  case class addPhotoToExistingAlbum()

  case class addImage()

  case class addAlbum()

  case class updateAlbum(a: Album)

  case class deleteAlbum(albumId: String)

  case class getProfileSecretKey(requestorId: String, pubKey: PublicKey)

  def startSchedulers(): Unit = {

    import system.dispatcher
    var scpost: Cancellable = system.scheduler.schedule(FiniteDuration.apply(1, "seconds"), FiniteDuration.apply(scUpdateTime.toLong, "seconds"), (new Runnable {
      def run {
        var userName = userPrefix + (Random.nextInt(postUserPer))
        println(userName + " scpost")
        var user = system.actorSelection(namingPrefix + userName)
        user ! UserPost("", userName, "post1 by " + userName, Option("google1"), Option("Paris"), Privacy.Friends, Some("uuid"))
      }
    }))

    var sccount = 1

    var scalbum: Cancellable = system.scheduler.schedule(FiniteDuration.apply(10, "seconds"), FiniteDuration.apply(scUpdateTime.toLong, "seconds"), (new Runnable {
      def run {
        sccount = sccount + 1
        var r = Random.nextInt(postPhotoPer)
        var userName = userPrefix + r
        println(userName + " scalbum")
        var user = system.actorSelection(namingPrefix + userName)
        user ! addDynamicAlbumAndPhoto()
      }
    }))

    var scphoto: Cancellable = system.scheduler.schedule(FiniteDuration.apply(12, "seconds"), FiniteDuration.apply(scUpdateTime.toLong, "seconds"), (new Runnable {
      def run {
        sccount = sccount + 1
        var r = Random.nextInt(postPhotoPer)
        var userName = userPrefix + r
        println(userName + " scphoto")
        var user = system.actorSelection(namingPrefix + userName)
        user ! addPhotoToExistingAlbum()
      }
    }))

    var scview: Cancellable = system.scheduler.schedule(FiniteDuration.apply(3, "seconds"), FiniteDuration.apply(scUpdateTime.toLong, "seconds"), (new Runnable {
      def run {
        sccount = sccount + 1
        var r = Random.nextInt(totalUsers)
        var userName = userPrefix + r
        println(userName + " scview")
        var user = system.actorSelection(namingPrefix + userName)
        user ! getProfile(userPrefix + Random.nextInt(Constants.totalUsers))
        user ! getPage()
      }
    }))

    var scfrndreq: Cancellable = system.scheduler.schedule(FiniteDuration.apply(100000, "seconds"), FiniteDuration.apply((2 * scUpdateTime).toLong, "seconds"), (new Runnable {
      def run {
        sccount = sccount + 1
        var r = Random.nextInt(Constants.totalUsers)
        var userName = userPrefix + r
        println(userName + " scfrndreq")
        var user = system.actorSelection(namingPrefix + userName)
        user ! FriendRequest(userName, userPrefix + (Random.nextInt(Constants.totalUsers)))
      }
    }))

    var scnewuser: Cancellable = system.scheduler.schedule(FiniteDuration.apply(20, "seconds"), FiniteDuration.apply((2 * scUpdateTime).toLong, "seconds"), (new Runnable {
      def run {
        val usercount = sccount + totalUsers
        var userName = userPrefix + usercount
        var key = Security.generateKey(userName)
        var publicKey = key.getPublic()
        var privateKey = key.getPrivate()
        var user = system.actorOf(Props(new UserClient(userName, publicKey, privateKey)), userName)
        println(userName + " scnewuser")
        var u = new User(userName, "First-" + userName, "Last-" + userName, Random.nextInt(100) + 1, Gender.apply(Random.nextInt(Gender.maxId)).toString(), Relation.Single.toString());
        user ! u
      }
    }))

    var scupdate: Cancellable = system.scheduler.schedule(FiniteDuration.apply(20, "seconds"), FiniteDuration.apply((3 * scUpdateTime).toLong, "seconds"), new Runnable {
      def run {
        sccount = sccount + 1
        var r = Random.nextInt(postPhotoPer)
        var userName = userPrefix + r
        var user = system.actorSelection(namingPrefix + userName)
        if (r % 2 == 0) {
          var a = new Album(userName, userName + "-defaultalbum", None, Some(System.currentTimeMillis().toString()), Option("initial album"), Option("Hyderabad"), Some(System.currentTimeMillis().toString()), None)
          user ? updateAlbum(a);
        } else {
          var u = new User(userName, "First-" + userName + "u", "Last-" + userName + "u", Random.nextInt(100) + 1, Gender.apply(Random.nextInt(Gender.maxId)).toString(), Relation.Single.toString());
          user ! updateProfile(u);
        }
      }
    })

    Thread.sleep(1000000)
    scpost.cancel()
    scalbum.cancel()
    scphoto.cancel()
    scview.cancel()
    scnewuser.cancel()
    scfrndreq.cancel()
    scupdate.cancel()
  }

  var createdUsers = new AtomicInteger(0)
  var frndsAdded = new AtomicInteger(0)
  var albumsAdded = new AtomicInteger(0)
  var photosAdded = new AtomicInteger(0)

  def bootSystem(): Unit = {
    createUsers()

    while (createdUsers.get() < Constants.totalUsers) {
      Thread.sleep(1000)
      println("Waiting till all the users have booted up!! : " + Constants.totalUsers + " users: " + createdUsers.get())
    }

    Thread.sleep(1000)
    makeFriends()

    while (frndsAdded.get() < Constants.totalUsers) {
      Thread.sleep(1000)
      println("Waiting till all the users have made some friends!! : " + Constants.totalUsers + " frndsAdded: " + frndsAdded.get())
    }

    Thread.sleep(1000)

    createAlbums()

    while (albumsAdded.get() < Constants.totalUsers) {
      Thread.sleep(1000)
      println("Waiting till all the users have at least one album!! : " + Constants.totalUsers + " albums: " + albumsAdded.get())
    }

    createPhotos()

    while (photosAdded.get() < Constants.totalUsers) {
      Thread.sleep(1000)
      println("Waiting till all the users have at least one album!! : " + Constants.totalUsers + " photos: " + photosAdded.get())
    }
  }

  def createUser(userId: String): Unit = {

    var key = Security.generateKey(userId)
    var publicKey = key.getPublic()
    var privateKey = key.getPrivate()
    var user = system.actorOf(Props(new UserClient(userId, publicKey, privateKey)), userId)
    var u = new User(userId, "First-" + userId, "Last-" + userId, Random.nextInt(100) + 1, Gender.apply(Random.nextInt(Gender.maxId)).toString(), Relation.Single.toString())
    user ? u
  }

  def createUsers(): Unit = {

    for (i <- 0 until Constants.totalUsers) {
      var userId = userPrefix + i
      createUser(userId)
    }
  }

  def makeFriends(): Unit = {

    println("Per user frnds : " + Constants.numOfFriends)

    for (i <- 0 until Constants.totalUsers) {
      var userId = userPrefix + i
      var user = system.actorSelection(namingPrefix + userId)

      var frndIds = Array[String]()

      var frndCount = 1
      while (frndCount < Constants.numOfFriends) {
        var frndId = userPrefix + ((i + frndCount) % Constants.totalUsers)
        frndIds = frndIds :+ frndId
        frndCount = frndCount + 1
      }
      var fList = frndIds.mkString(",")
      println("frndIds : " + frndIds.length + fList)

      var fr = new FriendRequest(userId, fList)
      user ! fr
    }
  }

  def createAlbums(): Unit = {
    for (i <- 0 until Constants.totalUsers) {
      var userId = userPrefix + i
      var user = system.actorSelection(namingPrefix + userId)
      user ! addDefaultAlbum()
    }
  }

  def createPhotos(): Unit = {
    for (i <- 0 until Constants.totalUsers) {
      var userId = userPrefix + i
      var user = system.actorSelection(namingPrefix + userId)
      user ! addDefaultImages()
    }
  }

  class UserClient(userId: String, publicKey: PublicKey, privateKey: PrivateKey) extends Actor with SprayJsonSupport with AdditionalFormats with ActorLogging {
    implicit val system = context.system

    import system.dispatcher

    implicit val timeout = Timeout(1000, TimeUnit.SECONDS)

    var pipeline = (sendReceive)

    def authenticate(): Boolean = {
      log.debug("Userid : " + userId + " key " + Security.getPublicKey(userId))
      var publicKeyStr = Base64.encodeBase64String(publicKey.getEncoded())
      var response: HttpResponse = Await.result(pipeline(Post(Constants.serverURL + "/auth/request", HttpEntity(MediaTypes.`application/json`, s"""{"userId": "$userId", "key" : "$publicKeyStr"}"""))), timeout.duration)
      var encryptedContent = response.entity.asString
      log.debug("To be decrypted : " + encryptedContent)
      var token = decryptUsingPrivate(encryptedContent)

      response = Await.result(pipeline(Post(Constants.serverURL + "/auth/verify", HttpEntity(MediaTypes.`application/json`, s"""{"userId": "$userId", "token" : "$token"}"""))), timeout.duration)
      if (response.entity.asString.toBoolean) {
        var httpCookie = response.headers.collect { case spray.http.HttpHeaders.`Set-Cookie`(hc) => hc }
        pipeline = addHeader(spray.http.HttpHeaders.Cookie(httpCookie)) ~> (sendReceive)
        log.info("Added cookie -> " + httpCookie + " to pipeline header!")
        true
      } else {
        log.error("Failed to authenticate :( " + userId)
        false
      }
    }

    //userId vs AES Key
    var profileKeys = new ConcurrentHashMap[String, String]()
    // post uuid vs AES Key
    var postKeys = new ConcurrentHashMap[String, String]()
    // photo uuid vs AES Key
    var photoKeys = new ConcurrentHashMap[String, String]()
    // user friends
    var userFriends = new ConcurrentHashMap[String, String]()
    // profile secret key the user profile.
    var profileSecretKey = "";

    def notifyToFrnds(PostType: String, uuid: String, secretKey: String) = {
      var it = userFriends.keySet().iterator()
      log.debug("Type : " + PostType + " uuid : " + uuid + " secretKey : " + secretKey)
      updateKeysMap(Notify(PostType, uuid, Security.encryptRSA(secretKey, publicKey)))
      while (it.hasNext) {
        var frndId = it.next()
        var frndPublicKey = Security.getPublicKey(frndId)
        var encryptedKey = Security.encryptRSA(secretKey, frndPublicKey)
        var frnd = system.actorSelection((namingPrefix + frndId))
        log.debug("Notifying friend : " + frndId + " of post " + PostType + " with uuid " + uuid)
        frnd ! Notify(PostType, uuid, encryptedKey)
      }
    }

    def updateKeysMap(n: Notify): Unit = {
      n.notifyType match {
        case Notification.ProfileType =>
          profileKeys.put(n.key, n.value)
        case Notification.PostType =>
          log.debug("Added " + n.notifyType + " key " + n.key)
          postKeys.put(n.key, n.value)
        case Notification.PhotoType =>
          photoKeys.put(n.key, n.value)
        case Notification.FriendAddType =>
          userFriends.put(n.key, n.value)
      }
    }

    import jsonProtocol._
    import spray.json._
    import DefaultJsonProtocol._

    def receive = {

      case u: User => {
        var auth = authenticate()

        if (auth) {
          log.info("Creating user with id " + userId)
          var random = new SecureRandom()
          var secretKey: Array[Byte] = Array.fill[Byte](16)(0)
          random.nextBytes(secretKey)
          profileSecretKey = new String(secretKey, Constants.charset)

          var firstName = Security.encryptProfileAES(u.firstName, publicKey, profileSecretKey)
          var lastName = u.lastName
          var age = u.age
          var gender = Security.encryptProfileAES(u.gender, publicKey, profileSecretKey)
          var relation = Security.encryptProfileAES(u.relation, publicKey, profileSecretKey)

          val result: Future[HttpResponse] = pipeline(Put(Constants.serverURL + "/user", HttpEntity(MediaTypes.`application/json`, s"""{"userId": "$userId", "firstName" : "$firstName" , "lastName" : "$lastName", "age" : $age , "gender" :  "$gender", "relation" :  "$relation"}""")))
          Await.result(result, timeout.duration)
          result.onComplete {
            x =>
              {
                var c = createdUsers.incrementAndGet()
                log.info("Current created users count : " + c)
                x.foreach { res => log.info(res.entity.asString) }
              }
          }
        } else {
          log.error(userId + " creation failed")
        }
      }

      case n: Notify => {
        log.debug("Received " + n.notifyType + " key " + n.key)
        updateKeysMap(n)
      }

      case sp: getProfileSecretKey => {
        log.debug("getProfileSecretKey req received from : " + sp.requestorId)
        var secretkey = Security.encryptRSA(profileSecretKey, sp.pubKey)
        sender ! secretkey
      }

      case uu: updateProfile => {
        var user = uu.u
        var userId = user.userId
        var firstName = Security.encryptProfileAES(user.firstName, publicKey, profileSecretKey)
        var lastName = user.lastName
        var age = user.age
        var gender = Security.encryptProfileAES(user.gender, publicKey, profileSecretKey)
        var relation = Security.encryptProfileAES(user.relation, publicKey, profileSecretKey)
        log.debug("Updating user with id " + userId)
        val result: Future[HttpResponse] = pipeline(Post(Constants.serverURL + "/user", HttpEntity(MediaTypes.`application/json`, s"""{"userId": "$userId", "firstName" : "$firstName" , "lastName" : "$lastName", "age" : $age , "gender" :  "$gender", "relation" :  "$relation"}""")))
        Await.result(result, timeout.duration)
        result.onComplete {
          x =>
            {
              x.foreach { res => log.debug(res.entity.asString) }
            }
        }
      }

      case rf: FriendRequest => {
        var userId = rf.userId
        var frndIds = rf.frndId
        log.debug("Requesting user,frnd : " + userId + " , " + frndIds)
        var frndIdsList = frndIds.split(",")
        var it = frndIdsList.iterator
        while (it.hasNext) {
          var frndId = it.next().trim()
          userFriends.put(frndId, frndId)
          var friend = system.actorSelection(namingPrefix + frndId)
          friend ! Notify(Notification.FriendAddType, userId, userId)
        }
        val result: Future[HttpResponse] = pipeline(Post(Constants.serverURL + "/user/" + userId + "/addfriend", HttpEntity(MediaTypes.`application/json`, s"""{"userId": "$userId" , "frndId" : "$frndIds" }""")))
        Await.result(result, timeout.duration)
        result.onComplete {
          x =>
            {
              frndsAdded.incrementAndGet()
              x.foreach { res => log.debug(res.entity.asString) }
            }
        }
      }

      case ap: UserPost => {

        var ar = Security.encryptAES(ap.message, publicKey)

        var message = StringEscapeUtils.escapeJson(ar.ciphedData)
        var link = ap.link
        var object_attachment = ap.object_attachment
        var place = ap.place
        var privacy = ap.privacy
        var postBy = ap.postby
        val postType = PostTypes.Default
        val result: HttpResponse = Await.result(pipeline(Put(Constants.serverURL + "/user/" + userId + "/feed", HttpEntity(MediaTypes.`application/json`, s"""{"uuid" : "" , "postby": "$postBy", "message": "$message", "link": "$link", "place": "$place", "privacy": "$privacy", "object_attachment": "$object_attachment" , "Type" : "$postType" }"""))), timeout.duration)

        log.debug("parsed json > " + result.entity.asString)
        try {
          var s = result.entity.asString.parseJson.convertTo[PostAdded]

          notifyToFrnds(Notification.PostType, s.uuid, ar.secretKey)

          log.debug(result.entity.asString)
        } catch {
          case t: Throwable => {
            t.printStackTrace()
            println("##############################"+result.entity.asString)
          }
        }
      }

      case gp: getProfile => {
        log.debug("profile query for " + gp.userId)
        val result: HttpResponse = Await.result(pipeline(Get(Constants.serverURL + "/user" + "?userId=" + gp.userId)), timeout.duration);
        var s = result.entity.asString.parseJson.convertTo[User]
        log.info("Profile received : " + s)
        var ciphedSecretkey = profileKeys.get(gp.userId)

        if (ciphedSecretkey == null) {
          var user = system.actorSelection(namingPrefix + gp.userId)
          ciphedSecretkey = Await.result(user ? getProfileSecretKey(userId, publicKey), timeout.duration).asInstanceOf[String]
          log.info("getProfileSecretKey req received from : " + gp.userId + " " + ciphedSecretkey)
          profileKeys.put(gp.userId, ciphedSecretkey)
        }
        if (ciphedSecretkey != null && !ciphedSecretkey.isEmpty()) {
          s.firstName = Security.decryptAES(ciphedSecretkey, s.firstName, privateKey)
          s.gender = Security.decryptAES(ciphedSecretkey, StringEscapeUtils.unescapeJson(s.gender), privateKey)
          s.relation = Security.decryptAES(ciphedSecretkey, s.relation, privateKey)
          log.info("Profile Decrypted : " + s)
        }
      }

      case gup: getPage => {
        val result: HttpResponse = Await.result(pipeline(Get(Constants.serverURL + "/user/" + userId + "/home")), timeout.duration)
        log.debug(result.entity.asString)
        try {
          import spray.json._
          log.debug("keys size post, photos, friends : " + postKeys.size() + "  " + photoKeys.size() + "  " + userFriends.size())
          log.debug("userpage : " + result.entity.asString)
          var userpage = result.entity.asString.parseJson.convertTo[UserPage]
          for (post <- userpage.posts) {
            // content will be encrypted. So decrypt and display the data/image here.
            if (post.Type.equals(Constants.PostTypes.Default)) {
              if (postKeys.containsKey(post.uuid)) {
                log.info("Encrypted Post : " + post)
                post.message = Security.decryptAES(postKeys.get(post.uuid), StringEscapeUtils.unescapeJson(post.message), privateKey)
                log.info("Decrypted Post : " + post)
              } else {
                log.info("No key to decrypt post Id : " + post.uuid + "size " + postKeys.size())
              }
            } else if (post.Type.equals(Constants.PostTypes.Photo)) {
              if (photoKeys.containsKey(post.uuid)) {
                log.info("Encrypted Photo Post : " + post)
                post.message = Security.decryptAES(photoKeys.get(post.uuid), post.message, privateKey)
                log.info("Decrypted Photo Post : " + post)
              } else {
                log.info("No key to decrypt photo Id : " + post.uuid + "size " + postKeys.size())
              }
            } else {
              log.error("Should not happen")
            }
          }
        } catch {
          case t: Throwable => {

          }
        }
      }

      case gfl: getFriendsList => {
        var userID = gfl.userId
        var frndId = gfl.frndId
        val result: Future[HttpResponse] = pipeline(Get(Constants.serverURL + "/user/" + userID + "/friendslist/" + frndId))
        Await.result(result, timeout.duration)
        result.onComplete {
          x =>
            {
              x.foreach { res => log.debug(res.entity.asString) }
            }
        }
      }

      case a: addDefaultAlbum => {
        var result = addAlbum(Album(userId, userId + "-defaultalbum", None, Some(System.currentTimeMillis().toString()), Option("initial album"), Option(Constants.places(Random.nextInt(Constants.places.length))), Some(System.currentTimeMillis().toString()), None))
        result.onComplete {
          x =>
            {
              albumsAdded.incrementAndGet()
              x.foreach { res => log.debug(res.entity.asString) }
            }
        }
      }

      case a: addDynamicAlbumAndPhoto => {
        var albumId = userId + UUID.randomUUID()
        var result = addAlbum(Album(userId, albumId, Some(userId + "-defaultphoto"), Some(System.currentTimeMillis().toString()), Option("dynamic album " + albumId), Option(Constants.places(Random.nextInt(Constants.places.length))), Some(System.currentTimeMillis().toString()), None))
        Await.result(result, timeout.duration)
        result.onComplete {
          x =>
            {
              x.foreach { res => log.debug(res.entity.asString) }
              var photoId = userId + UUID.randomUUID()
              var p = Photo(userId, albumId, photoId, readImage(), Some("Dynamic image" + photoId), Option(Constants.places(Random.nextInt(Constants.places.length))), false)
              var result = addImage(p)
              log.debug(result.entity.asString)
            }
        }
      }

      case a: addDefaultImages => {
        var src = readImage()
        //userId: String, albumId: String, photoId: String, src: String, message: Option[String] = None, place: Option[String] = None, noStory: Boolean = false)
        var p = Photo(userId, userId + "-defaultalbum", userId + "-defaultphoto", src, Some("Default first image"), Option(Constants.places(Random.nextInt(Constants.places.length))), false)
        var result = addImage(p)
        photosAdded.incrementAndGet()
        log.debug(result.entity.asString)
      }

      case p: addPhotoToExistingAlbum => {
        var albums = getUserAlbums(userId)
        //var albumId = albums(Random.nextInt(albums.length)).albumId
        var albumId = userId + "-defaultalbum"
        var photoId = userId + UUID.randomUUID()
        var p = Photo(userId, albumId, photoId, readImage(), Some("Dynamic image to existing album " + photoId), Option(Constants.places(Random.nextInt(Constants.places.length))), false)
        var result = addImage(p)
        log.debug(s"Photo added :\n${result.entity.asString}")
      }

      case a: getUserAlbums => {
        getUserAlbums(a.frndId)
      }

      case da: deleteAlbum => {
        val result: Future[HttpResponse] = pipeline(Delete(Constants.serverURL + "/user/" + userId + "/albums", "?albumId=" + da.albumId))
        Await.result(result, timeout.duration)
        result.onComplete {
          x =>
            {
              x.foreach { res =>
                log.debug(res.entity.asString)
              }
            }
        }
      }

      case ua: updateAlbum => {
        var a = ua.a
        var userId = a.userId
        var albumId = a.albumId
        var coverPhoto = if (a.coverPhoto.isDefined) a.coverPhoto.get else ""
        var createdTime = if (a.createdTime.isDefined) a.createdTime.get else ""
        var description = if (a.description.isDefined) a.description.get else ""
        var place = if (a.place.isDefined) a.place.get else ""
        var updateTime = if (a.updateTime.isDefined) a.updateTime.get else ""
        var result: Future[HttpResponse] = pipeline(Post(Constants.serverURL + "/user/" + userId + "/albums", HttpEntity(MediaTypes.`application/json`, s"""{"userId": "$userId", "albumId" :"$albumId", "coverPhoto" : "$coverPhoto", "createdTime" : "$createdTime", "description" : "$description", "place":"$place", "updateTime" :"$updateTime"}""")))
        Await.result(result, timeout.duration)
        result.onComplete {
          x =>
            {
              x.foreach { res =>
                log.debug(res.entity.asString)
              }
            }
        }
      }
    }

    var pubEncryptCipher = Cipher.getInstance(ALGORITHM)
    pubEncryptCipher.init(Cipher.ENCRYPT_MODE, publicKey)

    var pubDecryptCipher = Cipher.getInstance(ALGORITHM)
    pubDecryptCipher.init(Cipher.DECRYPT_MODE, publicKey)

    var priEncryptCipher = Cipher.getInstance(ALGORITHM)
    priEncryptCipher.init(Cipher.ENCRYPT_MODE, privateKey)

    var priDecryptCipher = Cipher.getInstance(ALGORITHM)
    priDecryptCipher.init(Cipher.DECRYPT_MODE, privateKey)

    def encryptUsingPublic(text: String): String = {
      var cipherText = pubEncryptCipher.doFinal(text.getBytes())
      new String(cipherText, Constants.charset)
    }

    def encryptUsingPrivate(text: String): String = {
      var cipherText = priEncryptCipher.doFinal(text.getBytes())
      new String(cipherText, Constants.charset)
    }

    def decryptUsingPrivate(text: String): String = {
      var dectyptedText = priDecryptCipher.doFinal(text.getBytes(Constants.charset))
      new String(dectyptedText)
    }

    def decryptUsingPublic(text: String): String = {
      var dectyptedText = pubDecryptCipher.doFinal(text.getBytes(Constants.charset))
      new String(dectyptedText)
    }

    def getUserAlbums(frndId: String): Array[Album] = {
      val result: Future[HttpResponse] = pipeline(Get(Constants.serverURL + "/user/" + userId + "/albums" + "?frndId=" + frndId))
      Await.result(result, timeout.duration)
      result.onComplete {
        x =>
          {
            x.foreach { res =>
              log.debug(res.entity.asString)
            }
          }
      }
      null
    }

    def addAlbum(a: Album): Future[HttpResponse] = {
      var userId = a.userId
      var albumId = a.albumId
      var coverPhoto = if (a.coverPhoto.isDefined) a.coverPhoto.get else ""
      var createdTime = if (a.createdTime.isDefined) a.createdTime.get else ""
      var description = if (a.description.isDefined) a.description.get else ""
      var place = if (a.place.isDefined) a.place.get else ""
      var updateTime = if (a.updateTime.isDefined) a.updateTime.get else ""
      pipeline(Put(Constants.serverURL + "/user/" + userId + "/albums", HttpEntity(MediaTypes.`application/json`, s"""{"userId": "$userId", "albumId" :"$albumId", "coverPhoto" : "$coverPhoto", "createdTime" : "$createdTime", "description" : "$description", "place":"$place", "updateTime" :"$updateTime"}""")))
    }

    def addImage(p: Photo): HttpResponse = {
      var userId = p.userId
      var albumId = p.albumId
      var photoId = p.photoId
      var aesRes = Security.encryptAES(readImage(), publicKey)
      var src = StringEscapeUtils.escapeJson(aesRes.ciphedData)
      var noStory = p.noStory
      var message = if (p.message.isDefined) p.message.get else ""
      var place = if (p.place.isDefined) p.place.get else ""
      var result = Await.result(pipeline(Put(Constants.serverURL + "/user/" + userId + "/albums/photo", HttpEntity(MediaTypes.`application/json`, s"""{"userId": "$userId", "albumId" : "$albumId", "place": "$place","photoId": "$photoId", "src": "$src", "message": "$message", "noStory": $noStory}"""))), timeout.duration)
      notifyToFrnds(Notification.PhotoType, p.photoId, aesRes.secretKey)
      result
    }

    //Image content will be encrypted and server does not know how to decrypt
    def readImage(): String = {
      var name = Constants.images(Random.nextInt(Constants.images.length))
      var byteArray = Files.readAllBytes(Paths.get(name))
      if (byteArray.length > 0) {
        var x = Base64.encodeBase64(byteArray)
        new String(x, Constants.charset)
      } else {
        log.error("No image found at : " + name)
        null
      }
    }
  }

}